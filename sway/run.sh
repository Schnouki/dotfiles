#!/usr/bin/env bash

_name=$(basename "$1")
exec systemd-run --user --scope --unit="sway-$_name-$$" -- "$@"
