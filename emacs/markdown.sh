#!/bin/sh

F=`mktemp`

cat > "$F"
markdown -x extra "$F"
rm -f "$F"