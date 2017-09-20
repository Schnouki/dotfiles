#!/usr/bin/env python

import requests
from bs4 import BeautifulSoup

URL = "http://fontawesome.io/cheatsheet/"

HEADER = """-- Auto-generated from {URL}, do not edit!
return {""".format(URL=URL)

FOOTER = """}"""

LINE = """    ["{name}"] = "{icon}","""

res = requests.get(URL)
soup = BeautifulSoup(res.text, "html.parser")

print(HEADER)

for row in soup.select(".page-header + .row > *"):
    icon_elt = row.find("i")
    icon = icon_elt.string
    name_elt = icon_elt.next_sibling
    name = name_elt.strip()
    if not name.startswith("fa-"):
        raise ValueError(row)
    print(LINE.format(icon=icon, name=name[3:]))

print(FOOTER)
