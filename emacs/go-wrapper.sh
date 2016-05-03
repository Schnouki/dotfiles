#!/usr/bin/env bash

export GOPATH="$1"
shift

exec go "$@"
