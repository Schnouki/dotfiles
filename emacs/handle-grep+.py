#!/usr/bin/env python

import os
import sys
from urllib.parse import unquote

url = unquote(sys.argv[1])
if not url.startswith("grep+://"):
    raise ValueError("Not a grep+ URL")

url = url[8:]

if ":" in url:
    path, line = url.rsplit(":", 1)
else:
    path, line = url, 1

command = ["eclient", f"+{line}", path]
os.execvp(command[0], command)
