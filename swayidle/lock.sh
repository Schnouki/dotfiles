#!/usr/bin/env bash

reason=$1
swaylock_args=()

if [[ "$reason" == "idle" ]]; then
    swaylock_args+=("--grace" 5)
fi

logger -t lock.sh "Syncing before lock..."
sync

logger -t lock.sh "Locking!"
exec swaylock -f "${swaylock_args[@]}"
