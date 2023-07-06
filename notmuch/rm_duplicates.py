#!/usr/bin/env python

import os
import os.path

import notmuch


def run(db):
    query = notmuch.Query(db, "")
    cnt = 0
    for msg in query.search_messages():
        fns = [fn for fn in msg.get_filenames()]
        if len(fns) < 2:
            continue

        dirnames = {os.path.dirname(os.path.dirname(fn)) for fn in fns}
        if len(dirnames) != 1:
            # print("Duplicates in different dirs: " + ", ".join(fns))
            continue

        fns.sort(key=os.path.getmtime)
        for fn in fns[1:]:
            cnt += 1
            # print(f"{fn} (dupe of {fns[0]}")
            os.unlink(fn)

    print(f"Deleted {cnt} duplicates.")


if __name__ == "__main__":
    db = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)
    run(db)
