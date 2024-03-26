#!/usr/bin/env bash

exec systemd-run --user --scope --unit="fuzzel-${FUZZEL_DESKTOP_FILE_ID/%.desktop/}-$$" -- "$@"
