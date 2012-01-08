#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import email.utils
import gtk
import notmuch
import pynotify
import sys

markup_cnt  = '[<span foreground="#93e0e3">{0}</span>]'
markup_from = '<span foreground="#ffeece">{0}</span>'
markup_subj = '<b>{0}</b>'
markup_tags = '<i>(<span foreground="#9fc59f">{0}</span>)</i>'

MAX_FROM_LEN = 35
MAX_SUBJ_LEN = 70

db = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)
q = notmuch.Query(db, "tag:unread or tag:todo")
q.set_sort(notmuch.Query.SORT.OLDEST_FIRST)

# Read data about the new messages
tab = []
msgs = 0
mlc, mlf, mls = 0, 0, 0
for thr in q.search_threads():
    from_ = unicode(thr.get_authors())
    subj_ = unicode(thr.get_subject())
    tags = list(thr.get_tags())
    for tag in ("unread", "todo"):
        try: tags.remove(tag)
        except ValueError: pass

    from_ = from_.split(u"|", 1)[0]

    lf = len(from_)
    if lf > MAX_FROM_LEN:
        from_ = from_[:MAX_FROM_LEN] + u"…"
        lf = len(from_)
    if lf > mlf:
        mlf = lf

    ls = len(subj_)
    if ls > MAX_SUBJ_LEN:
        subj_ = subj_[:MAX_SUBJ_LEN] + u"…"
        ls = len(subj_)
    if ls > mls:
        mls = ls

    from_ = from_.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
    subj_ = subj_.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
    tags = " ".join(tags).replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')

    count = thr.get_matched_messages()
    msgs += count
    count = markup_cnt.format(count)
    lc = len(count)
    if lc > mlc:
        mlc = lc
    
    from_ = markup_from.format(from_)
    subj_ = markup_subj.format(subj_)
    tags = markup_tags.format(tags)
    tab.append((count, from_, subj_, tags, lc, lf, ls))
    

if len(tab) == 0:
    sys.exit(0)

# Format a table
txt = []
for c, f, s, t, lc, lf, ls in tab:
    c += " " * (mlc-lc)
    f += " " * (mlf-lf)
    s += " " * (mls-ls)
    line = "{0} {1}  {2}  {3}".format(c, f, s, t)
    txt.append(line)
txt = '<span font_desc="DejaVu Sans Mono 7.5">' + "\n".join(txt) + '</span>'

# Display a notification
pynotify.init("notmuch notify")
n = pynotify.Notification("{0} unread messages in {1} threads".format(msgs, len(tab)), txt)
n.set_timeout(10000)
n.set_category("email.arrived")
icon = gtk.gdk.pixbuf_new_from_file("/usr/share/emacs/site-lisp/notmuch-logo.png")
n.set_icon_from_pixbuf(icon)
n.show()
