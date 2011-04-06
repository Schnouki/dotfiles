#!/usr/bin/env python2
# -*- coding: utf-8 -*-

from BeautifulSoup import BeautifulSoup
import email.header
import email.parser
import email.utils
import logging
import notmuch
import os.path
import re
import sys

_DBPATH = "~/.config/notmuch/addrbook.db"

try:
    import cPickle as pickle
except ImportError:
    import pickle

from profilehooks import profile

class AddrBook(object):
    def __init__(self, new=False):
        self._db = {}
        self._small_db = {}
        
        if not new:
            with open(os.path.expanduser(_DBPATH), "rb") as f:
                self._db = pickle.load(f)

    #@profile
    def index(self, msgs):
        tot_msg = 0
        tot_addr = 0
        try:
            parser = email.parser.Parser()
            for msg in msgs:
                fn = msg.get_filename()
                with open(fn, "r") as f:
                    mail = parser.parse(f, True)
                addrs = []
                for hdr in ("from", "to", "cc", "bcc"):
                    addrs += mail.get_all(hdr, [])
                addrs = email.utils.getaddresses(addrs)
                tot_addr += self._add(addrs)
                tot_msg += 1
                if (tot_msg % 20) == 0:
                    logging.debug("Messages: %d; addresses: %d" % (tot_msg, tot_addr))
        finally:
            # At the end, save the DB
            self._merge_db()
            with open(os.path.expanduser(_DBPATH), "wb") as f:
                pickle.dump(self._db, f, pickle.HIGHEST_PROTOCOL)
            logging.info("Total: indexed %d messages and %d addresses. %d unique addresses in the address book." % (tot_msg, tot_addr, len(self._db)))

    def _add(self, addrs):
        n = 0
        for user, addr in addrs:
            if len(addr) == 0:
                continue
            n += 1

            if len(user) > 0:
                dh = email.header.decode_header(user)[0]
                if dh[1] is not None:
                    try:
                        user = dh[0].decode(dh[1])
                    except Exception, e:
                        logging.warning(e)
                        logging.warning("Faulty data:", dh)
                else:
                    s = BeautifulSoup(dh[0])
                    user = unicode(s)

            if addr not in self._small_db:
                self._small_db[addr] = {}
            if user not in self._small_db[addr]:
                self._small_db[addr][user] = 1
            else:
                self._small_db[addr][user] += 1
        self._maybe_merge_db()
        return n

    def _maybe_merge_db(self):
        if len(self._small_db) >= 1000:
            self._merge_db()

    def _merge_db(self):
        for addr in self._small_db.iterkeys():
            if addr not in self._db:
                self._db[addr] = self._small_db[addr]
            else:
                for user in self._small_db[addr].iterkeys():
                    if user not in self._db[addr]:
                        self._db[addr][user] = self._small_db[addr][user]
                    else:
                        self._db[addr][user] += self._small_db[addr][user]
        self._small_db = {}

    def find(self, needle):
        # Init: construct a "flat DB" of "User <addr>" => addr. For each
        # address, only keep the 3 most frequent usernames.
        flat_db = {}
        for addr in self._db.iterkeys():
            users = self._db[addr].keys()
            _get_user_freq = lambda user: self._db[addr][user]
            users.sort(key=_get_user_freq, reverse=True)
            for user in users[:3]:
                full_addr = addr
                if len(user) > 0:
                    full_addr = "%s <%s>" % (user, addr)
                flat_db[full_addr] = addr
            
        # Step 1: find matching entries
        needle = unicode(BeautifulSoup(needle)).lower()
        matches = filter(lambda s: s.lower().find(needle) >= 0, flat_db.iterkeys())

        # Step 2: get matching addresses
        match_addr = []
        for fa in matches:
            addr = flat_db[fa]
            if addr not in match_addr:
                match_addr.append(addr)

        # Step 3: sort by frequency
        _get_addr_freq = lambda addr: sum(self._db[addr].values())
        match_addr.sort(key=_get_addr_freq, reverse=True)

        # Step 4: display results
        match_addr = match_addr[:25]
        for addr in match_addr:
            _get_user_freq = lambda user: self._db[addr][user]
            users = self._db[addr].keys()
            users.sort(key=_get_user_freq, reverse=True)
            user = users[0]
            if len(user) > 0:
                full = u"%s <%s>" % (user, unicode(addr))
                print full.encode("utf-8")
            else:
                print addr.encode("utf-8")

if __name__ == "__main__":
    logging.basicConfig(level=logging.INFO,
                        format="%(levelname)s: %(message)s")

    # If called without any argument, this is supposed to be a first run: index everything
    if len(sys.argv) == 1:
        if os.path.exists(os.path.expanduser(_DBPATH)):
            print "%s already exists.\nPlease delete it if you want to recreate the database." % _DBPATH
            sys.exit(1)

        ab = AddrBook(True)
        nm = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)
        query = notmuch.Query(nm, "tag:sent or tag:replied")
        ab.index(query.search_messages())

    # If called with arguments, search for that.
    else:
        needle = " ".join(sys.argv[1:])
        ab = AddrBook()
        res = ab.find(needle)
