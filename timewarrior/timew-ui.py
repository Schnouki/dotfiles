#!/usr/bin/env python

from dataclasses import dataclass
import json
from pathlib import Path
import subprocess as subp

import gi

gi.require_version("Gtk", "3.0")
from gi.repository import Gtk

RESP_STOP = 42
RESP_EDIT = 43
RESP_START = 44

PREVIOUS_COLUMNS = 2
PREVIOUS_ROWS = 5


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
    return list(tags_data.keys())


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


def main():
    all_tags = get_tags()
    st = TWStatus.get()
    prev_tags = get_prev_tags()

    # Tags view management
    tags_store = Gtk.ListStore(str)
    tags_iters = {}
    for tw_tag in all_tags:
        tag_iter = tags_store.append([tw_tag])
        tags_iters[tw_tag] = tag_iter
    tags_store.append([""])

    tree_view = Gtk.TreeView(model=tags_store, headers_visible=False, reorderable=False)
    sel = tree_view.get_selection()
    sel.set_mode(Gtk.SelectionMode.MULTIPLE)

    cell = Gtk.CellRendererText(editable=True)

    def on_edit(cr, path, new_text):
        tree_iter = tags_store.get_iter_from_string(path)
        prev_text = tags_store.get_value(tree_iter, 0)
        if prev_text == new_text:
            return

        tags_store.set_value(tree_iter, 0, new_text)
        if prev_text == "":
            tags_store.append([""])

    cell.connect("edited", on_edit)

    col = Gtk.TreeViewColumn("Tag", cell, text=0)
    tree_view.append_column(col)

    # Helper: select a list of tags
    def select_tags(tags: list[str]):
        sel.unselect_all()
        for tw_tag in tags:
            tag_iter = tags_iters[tw_tag]
            sel.select_iter(tag_iter)

    def on_select_tags_btn(btn, tags: list[str]):
        select_tags(tags)

    # Build the dialog
    dlg = Gtk.Dialog(title="Timewarrior", modal=True, use_header_bar=True)
    dlg.set_resizable(False)

    content = dlg.get_content_area()
    box = Gtk.Box(orientation=Gtk.Orientation.VERTICAL, spacing=6)
    content.add(box)

    dlg.add_button("Start", RESP_START)
    if not st.stopped:
        dlg.add_button("Edit", RESP_EDIT)
        dlg.add_button("Stop", RESP_STOP)
    dlg.add_button(Gtk.STOCK_CANCEL, Gtk.ResponseType.CANCEL)
    dlg.set_default_response(RESP_START)

    prev_frm = Gtk.Frame(label="Previous entries")
    box.pack_start(prev_frm, True, True, 0)
    prev_grid = Gtk.Grid(column_homogeneous=True)
    prev_frm.add(prev_grid)

    for i, btn_tags in enumerate(prev_tags):
        btn = Gtk.Button(label=", ".join(sorted(btn_tags)))
        btn.connect("clicked", on_select_tags_btn, btn_tags)
        top, left = divmod(i, PREVIOUS_COLUMNS)
        prev_grid.attach(btn, left, top, 1, 1)

    next_frm = Gtk.Frame(label="Tags for next entry")
    box.pack_start(next_frm, True, True, 0)
    next_frm.add(tree_view)
    tree_view.grab_focus()

    if st.tags:
        select_tags(st.tags)

    # Run!
    dlg.show_all()
    resp = dlg.run()

    if resp == RESP_START:
        model, paths = sel.get_selected_rows()
        tags = [model.get_value(model.get_iter(path), 0) for path in paths]
        subp.run(["timew", "start"] + tags, check=True)

    elif resp == RESP_EDIT:
        model, paths = sel.get_selected_rows()
        all_new_tags = [model.get_value(model.get_iter(path), 0) for path in paths]
        new_tags = [tag for tag in all_new_tags if tag not in st.tags]
        old_tags = [tag for tag in st.tags if tag not in all_new_tags]

        if new_tags:
            subp.run(["timew", "tag", "@1"] + new_tags)
        if old_tags:
            subp.run(["timew", "untag", "@1"] + old_tags)

    elif resp == RESP_STOP:
        subp.run(["timew", "stop"])


if __name__ == "__main__":
    main()
