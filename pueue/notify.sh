#!/usr/bin/env bash

set -euo pipefail

id="$1"
command="$2"
path="$3"
result="$4"
exit_code="$5"
group="$6"
output="$7"
start="$8"
end="$9"

if [[ "$result" == "Success" ]] && [[ "$group" == "git" ]]; then
    exit 0
fi

time=$(( end - start ))
command_name=$(basename "${command%% *}")

exec notify-send "Pueue: task $id: $result" "$command_name finished with code $exit_code in $time seconds."
