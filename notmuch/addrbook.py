#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import email.header
import email.parser
import email.utils
import logging
import pickle
import os.path
import sys
import typing

from bs4 import BeautifulSoup
import notmuch

_DBPATH = "~/.config/notmuch/addrbook.db"

#from profilehooks import profile

class AddrBook(object):
    """An address book"""

    def __init__(self, new=False):
        self._db = {}
        self._small_db = {}

        if not new:
            with open(os.path.expanduser(_DBPATH), "rb") as data:
                self._db = pickle.load(data)

    #@profile
    def index(self, msgs: notmuch.Messages) -> None:
        """Index messages in the address book"""
        tot_msg = 0
        tot_addr = 0
        try:
            parser = email.parser.Parser()
            for msg in msgs:
                msg_fn = msg.get_filename()
                with open(msg_fn, "r") as data:
                    mail = parser.parse(data, True)
                addrs = []
                for hdr in ("from", "to", "cc", "bcc"):
                    addrs += mail.get_all(hdr, [])
                addrs = email.utils.getaddresses(addrs)
                tot_addr += self._add(addrs)
                tot_msg += 1
                if (tot_msg % 20) == 0:
                    logging.debug("Messages: %d; addresses: %d", tot_msg, tot_addr)
        finally:
            # At the end, save the DB
            self._merge_db()
            with open(os.path.expanduser(_DBPATH), "wb") as fout:
                pickle.dump(self._db, fout, pickle.HIGHEST_PROTOCOL)
            logging.info("Total: indexed %d messages and %d addresses. "
                         "%d unique addresses in the address book.",
                         tot_msg, tot_addr, len(self._db))

    def _add(self, addrs: typing.List[typing.Tuple[str, str]]) -> int:
        cnt = 0
        for user, addr in addrs:
            if len(addr) == 0:
                continue
            cnt += 1

            if len(user) > 0:
                hdr = email.header.decode_header(user)[0]
                if hdr[1] is not None:
                    try:
                        user = hdr[0].decode(hdr[1])
                    except Exception as e:
                        logging.warning(e)
                        logging.warning("Faulty data: %s", hdr)
                else:
                    user = str(BeautifulSoup(hdr[0], "lxml"))

            if addr not in self._small_db:
                self._small_db[addr] = {}
            if user not in self._small_db[addr]:
                self._small_db[addr][user] = 1
            else:
                self._small_db[addr][user] += 1
        self._maybe_merge_db()
        return cnt

    def _maybe_merge_db(self):
        if len(self._small_db) >= 1000:
            self._merge_db()

    def _merge_db(self):
        for addr in self._small_db:
            if addr not in self._db:
                self._db[addr] = self._small_db[addr]
            else:
                for user in self._small_db[addr]:
                    if user not in self._db[addr]:
                        self._db[addr][user] = self._small_db[addr][user]
                    else:
                        self._db[addr][user] += self._small_db[addr][user]
        self._small_db = {}

    def find(self, needle: str) -> None:
        """Find entries in the address book that match the provided string"""
        # Init: construct a "flat DB" of "User <addr>" => addr. For each
        # address, only keep the 3 most frequent usernames.
        flat_db = {}
        def _get_user_freq(addr: str) -> typing.Callable[[str], str]:
            return lambda user: self._db[addr][user]
        for addr in self._db:
            users = list(self._db[addr].keys())
            users.sort(key=_get_user_freq(addr), reverse=True)
            for user in users[:3]:
                full_addr = addr
                if len(user) > 0:
                    full_addr = "%s <%s>" % (user, addr)
                flat_db[full_addr] = addr

        # Step 1: find matching entries
        #needle = (BeautifulSoup(needle)).lower()
        needle = needle.lower()
        matches = filter(lambda s: s.lower().find(needle) >= 0, flat_db.keys())

        # Step 2: get matching addresses
        match_addr = []
        for full_addr in matches:
            addr = flat_db[full_addr]
            if addr not in match_addr:
                match_addr.append(addr)

        # Step 3: sort by frequency
        _get_addr_freq = lambda addr: sum(self._db[addr].values())
        match_addr.sort(key=_get_addr_freq, reverse=True)

        # Step 4: display results
        match_addr = match_addr[:25]
        for addr in match_addr:
            _get_user_freq = lambda user: self._db[addr][user]
            users = list(self._db[addr].keys())
            users.sort(key=_get_user_freq, reverse=True)
            user = users[0]
            if len(user) > 0:
                full = "%s <%s>" % (user, addr)
                print(full)
            else:
                print(addr)

def _main():
    logging.basicConfig(level=logging.INFO,
                        format="%(levelname)s: %(message)s")

    # If called without any argument, this is supposed to be a first run: index everything
    if len(sys.argv) == 1:
        if os.path.exists(os.path.expanduser(_DBPATH)):
            print("%s already exists.\n"
                  "Please delete it if you want to recreate the database." % _DBPATH)
            sys.exit(1)

        book = AddrBook(True)
        nmdb = notmuch.Database(mode=notmuch.Database.MODE.READ_ONLY)
        query = notmuch.Query(nmdb, "tag:sent or tag:replied")
        book.index(query.search_messages())

    # If called with arguments, search for that.
    else:
        needle = " ".join(sys.argv[1:])
        AddrBook().find(needle)


if __name__ == "__main__":
    _main()
