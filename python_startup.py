# Frequently used stdlib modules
import csv
import hashlib
import itertools as it
import json
import os
import random
import random
import re
import subprocess as subp
import sys
import tempfile
from datetime import datetime, timedelta
from glob import glob
from pathlib import Path
from pprint import pprint
from uuid import uuid4

# Add venv name to prompt
if env := os.environ.get("VIRTUAL_ENV"):
    env_name = os.path.basename(env)
    sys.ps1 = f"({env_name}) {getattr(sys, 'ps1', '>>> ')}"

# Quick aliases
p = print
pp = pprint

# Allow pasting JSON easily
# https://chaos.social/@citizen428/111545494213820598
null = None
true = True
false = False
