#!/usr/bin/env bash

ELISP_FILE_NAME="$1"
shift

echo 400 > /proc/self/oom_score_adj

exec systemd-run --user --scope \
     -p MemoryMax=3G \
     --description="Emacs sub-process started from ${ELISP_FILE_NAME}" \
     "$@"
