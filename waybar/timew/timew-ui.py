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

@dataclass(frozen=True)
class TWStatus:
    stopped: bool
    tags: list[str]

    @classmethod
    def get(cls) -> "TWStatus":
        pr = subp.run(["timew", "get", "dom.tracked.1.json"], capture_output=True, check=True)
        data = json.loads(pr.stdout)
        return cls(stopped="end" in data, tags=data.get("tags", []))


def get_tags() -> list[str]:
    tags_path = Path.home() / ".timewarrior" / "data" / "tags.data"
    with tags_path.open() as fd:
        tags_data = json.load(fd)
    return list(tags_data.keys())



def main():
    all_tags = get_tags()
    st = TWStatus.get()

    dlg = Gtk.Dialog(title="Timewarrior", modal=True, use_header_bar=True)
    dlg.set_resizable(False)

    content = dlg.get_content_area()

    dlg.add_button("Start", RESP_START)
    if not st.stopped:
        dlg.add_button("Edit", RESP_EDIT)
        dlg.add_button("Stop", RESP_STOP)
    dlg.add_button(Gtk.STOCK_CANCEL, Gtk.ResponseType.CANCEL)
    dlg.set_default_response(RESP_START)

    tags_store = Gtk.ListStore(str)
    tags_iters = {}
    for tw_tag in all_tags:
        tag_iter = tags_store.append([tw_tag])
        tags_iters[tw_tag] = tag_iter
    tags_store.append([""])

    view = Gtk.TreeView(model=tags_store, headers_visible=False, reorderable=False)
    content.add(view)

    sel = view.get_selection()
    sel.set_mode(Gtk.SelectionMode.MULTIPLE)
    if st.tags:
        for tw_tag in st.tags:
            tag_iter = tags_iters[tw_tag]
            sel.select_iter(tag_iter)

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
    view.append_column(col)

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
