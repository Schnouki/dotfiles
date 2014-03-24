#!/usr/bin/env python2
# -*- coding: utf-8 -*-

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

        dirnames = [os.path.dirname(os.path.dirname(fn)) for fn in fns]
        same_dir = True
        for dirname in dirnames:
            if dirname != dirnames[0]:
                same_dir = False
                break
        if not same_dir:
            continue

        fns.sort(key=os.path.getmtime)
        for fn in fns[1:]:
            cnt += 1
            os.unlink(fn)

    print cnt

if __name__ == "__main__":
    db = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)
    run(db)
