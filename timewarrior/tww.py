#!/usr/bin/python3

import ctypes
from datetime import datetime, UTC
import enum
import json
import select
import subprocess as subp
import os

import psutil

TIMEOUT_MS = 30_000
BLOCK_SIZE = 1024


class INotify:
    libc = ctypes.CDLL("libc.so.6")

    class WatchFlag(enum.IntFlag):
        MODIFY = 0x00000002
        CLOSE_WRITE = 0x00000008

    # C functions
    _init1 = libc.inotify_init1
    _init1.restype = ctypes.c_int
    _init1.argtypes = [ctypes.c_int]

    _add_watch = libc.inotify_add_watch
    _add_watch.restype = ctypes.c_int
    _add_watch.argtypes = [ctypes.c_int, ctypes.c_char_p, ctypes.c_uint32]

    # Python wrapper
    def __init__(self):
        IN_NONBLOCK = 0o00004000
        self.fd = self._init1(IN_NONBLOCK)

    def add_watch(self, path: str, mask: WatchFlag) -> int:
        wd = self._add_watch(self.fd, path.encode(), mask)
        if wd < 0:
            errno = ctypes.get_errno()
            raise OSError(errno, os.strerror(errno))
        return wd


class Watcher:
    def __init__(self, path: str):
        self.path = os.path.expanduser(path)
        self.ino = INotify()
        self.poll = select.poll()
        self.poll.register(self.ino.fd, select.POLLIN)
        self.wd = self.ino.add_watch(self.path, INotify.WatchFlag.MODIFY)

    def flush_ino(self):
        try:
            while data := os.read(self.ino.fd, BLOCK_SIZE):
                if len(data) < BLOCK_SIZE:
                    return
        except BlockingIOError:
            pass

    def update(self):
        procs = [
            proc for proc in psutil.process_iter(["name"]) if proc.name() == "timew"
        ]
        if procs:
            psutil.wait_procs(procs, timeout=1)

        try:
            cp = subp.run(
                ["timew", "get", "dom.tracked.1.json"], check=True, capture_output=True
            )
        finally:
            self.flush_ino()

        interval = json.loads(cp.stdout)
        text = ""
        tags = interval.get("tags", [])

        if "end" in interval:
            text = "inactive"
        else:
            start = datetime.fromisoformat(interval["start"])
            duration = (datetime.now(UTC) - start).total_seconds()
            mm = int(duration) // 60
            hh, mm = divmod(mm, 60)
            text = f"{hh}:{mm:02d}"
            if tags:
                text += f" <small>[{', '.join(tags)}]</small>"

        print(json.dumps({"text": text}), flush=True)

    def run(self):
        self.update()
        while True:
            ev = self.poll.poll(TIMEOUT_MS)
            self.update()


if __name__ == "__main__":
    w = Watcher("~/.local/share/timewarrior/data")
    w.run()
