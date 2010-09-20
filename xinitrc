#!/bin/bash

# GTK-2.0 RC files
export GTK2_RC_FILES="$HOME/.config/gtkrc-2.0:$HOME/.config/gtkrc-$(hostname)"

# Correct keyboard layout
if [ -f "$HOME/.Xmodmap" ]; then
	xmodmap "$HOME/.Xmodmap"
fi

# VNC server
x11vnc -o /var/log/x11vnc.log -forever -bg -localhost -solid black -avahi -unixpw schnouki

# NVIDIA card configuration
nvidia-settings -l

# GPG agent
gnupginf="$HOME/.gnupg/gpg-agent.info"
if ( pgrep -u "${USER}" gpg-agent ); then
    eval `cat $gnupginf`
    eval `cut -d= -f1 $gnupginf | xargs echo export`
else
    eval `gpg-agent --daemon`
fi

# Screensaver
[[ $(hostname) != "odin" ]] && xscreensaver -no-splash &

# Tell Java we have a "non-reparenting window manager"
wmname LG3D

# Emacs daemon
EMACSPIDFILE="$HOME/.config/emacs/.emacs.desktop.lock"
if [ -e "$EMACSPIDFILE" ]; then
    epid=$(cat "$EMACSPIDFILE")
    if ! [ -e "/proc/$epid/exe" -a $(readline "/proc/$epid/exe") = "/usr/bin/emacs" ]; then
        rm "$EMACSPIDFILE"
        emacs --daemon
    fi
fi

# Useless apps to run at boot...
gnote &
#gajim &
#thunderbird &
#firefox &

exec awesome
#exec twm
