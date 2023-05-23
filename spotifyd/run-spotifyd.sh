#!/usr/bin/env bash

exec env \
     PULSE_PROP_application.name=spotifyd \
     PULSE_PROP_application.icon_name=spotify \
     PULSE_PROP_media.role=music \
     /usr/bin/spotifyd "$@"
