#!/usr/bin/env bash

logger -t lock.sh "Syncing before lock..."
sync

logger -t lock.sh "Locking!"
exec swaylock -f -c 000000
