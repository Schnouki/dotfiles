#!/usr/bin/env python

from dataclasses import dataclass
from datetime import datetime, timedelta
import json
from pathlib import Path
import subprocess as subp

import gi

gi.require_version("Gtk", "4.0")
from gi.repository import Gtk, Gdk

PREVIOUS_COLUMNS = 2
PREVIOUS_ROWS = 5

# UI Layout Constants
UI_BTN_SPACING = 6
UI_GRID_PADDING = 12
UI_FRAME_PADDING = 12
UI_SECTION_SPACING = 12
UI_DIALOG_PADDING = 12


@dataclass(frozen=True)
class TWStatus:
    stopped: bool
    tags: list[str]

    @classmethod
    def get(cls, n=1) -> "TWStatus":
        pr = subp.run(
            ["timew", "get", f"dom.tracked.{n}.json"], capture_output=True, check=True
        )
        data = json.loads(pr.stdout)
        return cls(stopped="end" in data, tags=data.get("tags", []))


def get_tags() -> list[str]:
    tags_path = Path.home() / ".local" / "share" / "timewarrior" / "data" / "tags.data"
    with tags_path.open() as fd:
        tags_data = json.load(fd)
    return sorted(list(tags_data.keys()))


def get_prev_tags() -> list[frozenset[str]]:
    pr = subp.run(
        ["timew", "get", "dom.tracked.count"], capture_output=True, check=True
    )
    max_n = int(pr.stdout)
    nb_previous = PREVIOUS_COLUMNS * PREVIOUS_ROWS

    tag_sets = list()
    for n in range(1, max_n + 1):
        status = TWStatus.get(n)
        tag_set = frozenset(status.tags)
        if tag_set not in tag_sets:
            tag_sets.append(tag_set)
        if len(tag_sets) >= nb_previous:
            break
    return tag_sets


class TimewApp(Gtk.Application):
    def __init__(self):
        super().__init__(application_id="org.schnouki.timew-ui")
        self.all_tags = []
        self.status = None
        self.prev_tags = []
        self.tag_rows = {}
        self.result_tags = []
        self.result_action = None

    def do_activate(self):
        self.all_tags = get_tags()
        self.status = TWStatus.get()
        self.prev_tags = get_prev_tags()

        win = Gtk.ApplicationWindow(application=self, title="Timewarrior")
        win.set_resizable(False)

        key_controller = Gtk.EventControllerKey()
        key_controller.connect("key-pressed", self.on_key_pressed)
        win.add_controller(key_controller)

        header = Gtk.HeaderBar()
        win.set_titlebar(header)

        cancel_btn = Gtk.Button(label="Cancel")
        cancel_btn.connect("clicked", lambda b: self.quit())
        header.pack_start(cancel_btn)

        start_btn = Gtk.Button(label="Start")
        start_btn.add_css_class("suggested-action")
        start_btn.connect("clicked", self.on_start)
        header.pack_end(start_btn)

        if not self.status.stopped:
            stop_btn = Gtk.Button(label="Stop")
            stop_btn.add_css_class("destructive-action")
            stop_btn.connect("clicked", self.on_stop)
            header.pack_end(stop_btn)

            edit_btn = Gtk.Button(label="Edit")
            edit_btn.connect("clicked", self.on_edit)
            header.pack_end(edit_btn)

        main_box = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=UI_SECTION_SPACING)
        main_box.set_margin_top(UI_DIALOG_PADDING)
        main_box.set_margin_bottom(UI_DIALOG_PADDING)
        main_box.set_margin_start(UI_DIALOG_PADDING)
        main_box.set_margin_end(UI_DIALOG_PADDING)
        win.set_child(main_box)

        # Time shift
        shift_box = Gtk.Box(orientation=Gtk.Orientation.HORIZONTAL, spacing=UI_BTN_SPACING)
        shift_box.set_halign(Gtk.Align.CENTER)
        main_box.append(shift_box)

        shift_label = Gtk.Label(label="Time shift (minutes ago):")
        shift_box.append(shift_label)

        self.shift_spin = Gtk.SpinButton()
        self.shift_spin.set_range(0, 120)
        self.shift_spin.set_increments(1, 5)
        self.shift_spin.set_value(0)
        shift_box.append(self.shift_spin)

        # Previous entries frame
        prev_frame = Gtk.Frame(label="Previous entries")
        main_box.append(prev_frame)

        prev_grid = Gtk.Grid()
        prev_grid.set_column_homogeneous(True)
        prev_grid.set_row_spacing(UI_BTN_SPACING)
        prev_grid.set_column_spacing(UI_BTN_SPACING)
        prev_grid.set_margin_top(UI_GRID_PADDING)
        prev_grid.set_margin_bottom(UI_GRID_PADDING)
        prev_grid.set_margin_start(UI_GRID_PADDING)
        prev_grid.set_margin_end(UI_GRID_PADDING)
        prev_frame.set_child(prev_grid)

        for i, btn_tags in enumerate(self.prev_tags):
            btn = Gtk.Button(label=", ".join(sorted(btn_tags)))
            btn.connect("clicked", self.on_select_tags_btn, btn_tags)
            top, left = divmod(i, PREVIOUS_COLUMNS)
            prev_grid.attach(btn, left, top, 1, 1)

        # Tags frame with ListBox
        next_frame = Gtk.Frame(label="Tags for next entry")
        main_box.append(next_frame)

        frame_box = Gtk.Box(orientation=Gtk.Orientation.VERTICAL)
        frame_box.set_margin_top(UI_FRAME_PADDING)
        frame_box.set_margin_bottom(UI_FRAME_PADDING)
        frame_box.set_margin_start(UI_FRAME_PADDING)
        frame_box.set_margin_end(UI_FRAME_PADDING)
        next_frame.set_child(frame_box)

        scrolled = Gtk.ScrolledWindow()
        scrolled.set_policy(Gtk.PolicyType.NEVER, Gtk.PolicyType.AUTOMATIC)
        scrolled.set_propagate_natural_height(True)
        frame_box.append(scrolled)

        self.list_box = Gtk.ListBox()
        self.list_box.set_selection_mode(Gtk.SelectionMode.MULTIPLE)
        scrolled.set_child(self.list_box)

        for tag in self.all_tags:
            row = self.create_tag_row(tag)
            self.list_box.append(row)
            self.tag_rows[tag] = row

        # Add empty row for new tags
        self.new_tag_row = self.create_tag_row("", editable=True)
        self.list_box.append(self.new_tag_row)

        # Select current tags
        if self.status.tags:
            self.select_tags(self.status.tags)

        win.present()

    def create_tag_row(self, tag: str, editable: bool = False) -> Gtk.ListBoxRow:
        row = Gtk.ListBoxRow()
        if editable:
            entry = Gtk.Entry()
            entry.set_text(tag)
            entry.set_placeholder_text("Add new tag...")
            entry.connect("activate", self.on_new_tag_activate)
            row.set_child(entry)
        else:
            label = Gtk.Label(label=tag)
            label.set_halign(Gtk.Align.START)
            row.set_child(label)
        return row

    def on_key_pressed(self, controller, keyval, keycode, state):
        if keyval == Gdk.KEY_Escape:
            self.quit()
            return True
        return False

    def on_new_tag_activate(self, entry):
        new_tag = entry.get_text().strip()
        if new_tag and new_tag not in self.tag_rows:
            row = self.create_tag_row(new_tag)
            self.list_box.insert(row, len(self.tag_rows))
            self.tag_rows[new_tag] = row
            self.all_tags.append(new_tag)
            entry.set_text("")
            self.list_box.select_row(row)

    def select_tags(self, tags: list[str]):
        self.list_box.unselect_all()
        for tag in tags:
            if tag in self.tag_rows:
                self.list_box.select_row(self.tag_rows[tag])

    def on_select_tags_btn(self, btn, tags):
        self.select_tags(tags)

    def get_selected_tags(self) -> list[str]:
        tags = []
        for row in self.list_box.get_selected_rows():
            child = row.get_child()
            if isinstance(child, Gtk.Label):
                tags.append(child.get_text())
            elif isinstance(child, Gtk.Entry):
                text = child.get_text().strip()
                if text:
                    tags.append(text)
        return tags

    def get_action_time(self) -> str:
        shift_minutes = int(self.shift_spin.get_value())
        return (datetime.now() - timedelta(minutes=shift_minutes)).isoformat(timespec="seconds")

    def on_start(self, btn):
        tags = self.get_selected_tags()
        action_time = self.get_action_time()
        subp.run(["timew", "start", action_time] + tags, check=True)
        self.quit()

    def on_edit(self, btn):
        all_new_tags = self.get_selected_tags()
        new_tags = [tag for tag in all_new_tags if tag not in self.status.tags]
        old_tags = [tag for tag in self.status.tags if tag not in all_new_tags]

        if new_tags:
            subp.run(["timew", "tag", "@1"] + new_tags)
        if old_tags:
            subp.run(["timew", "untag", "@1"] + old_tags)
        self.quit()

    def on_stop(self, btn):
        action_time = self.get_action_time()
        subp.run(["timew", "stop", action_time])
        self.quit()


def main():
    app = TimewApp()
    app.run(None)


if __name__ == "__main__":
    main()
