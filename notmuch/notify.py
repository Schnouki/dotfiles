#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import email.utils
import gtk
import notmuch
import pynotify
import sys

markup_from = '<span foreground="#ffeece">{0}</span>'
markup_subj = '<b>{0}</b>'
markup_tags = '<i>(<span foreground="#9fc59f">{0}</span>)</i>'

db = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)
q = notmuch.Query(db, "tag:unread")
q.set_sort(notmuch.Query.SORT.NEWEST_FIRST)

# Read data about the new messages
tab = []
mlf, mls = 0, 0
for msg in q.search_messages():
    from_ = unicode(msg.get_header("from"))
    f = email.utils.parseaddr(from_)
    if len(f[0]) > 0:
        from_ = f[0]
    elif len(f[1]) > 0:
        from_ = f[1]
    subj_ = unicode(msg.get_header("subject"))
    tags = list(msg.get_tags())
    tags.remove("unread")

    lf = len(from_)
    if lf > mlf:
        mlf = lf
    ls = len(subj_)
    if ls > mls:
        mls = ls

    from_ = from_.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
    subj_ = subj_.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
    tags = " ".join(tags).replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')

    from_ = markup_from.format(from_)
    subj_ = markup_subj.format(subj_)
    tags = markup_tags.format(tags)
    tab.append((from_, subj_, tags, lf, ls))

if len(tab) == 0:
    sys.exit(0)

# Format a table
txt = []
for f, s, t, lf, ls in tab:
    f += " " * (mlf-lf)
    s += " " * (mls-ls)
    line = "{0}  {1}  {2}".format(f, s, t)
    txt.append(line)
txt = '<span font_desc="DejaVu Sans Mono 7.5">' + "\n".join(txt) + '</span>'

# Display a notification
pynotify.init("notmuch notify")
n = pynotify.Notification("{0} unread messages".format(len(tab)), txt)
n.set_timeout(10000)
n.set_category("email.arrived")
icon = gtk.gdk.pixbuf_new_from_file("/usr/share/emacs/site-lisp/notmuch-logo.png")
n.set_icon_from_pixbuf(icon)
n.show()
