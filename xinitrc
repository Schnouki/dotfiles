#!/bin/bash

# GTK-2.0 RC files
export GTK2_RC_FILES="$HOME/.config/gtkrc-2.0:$HOME/.config/gtkrc-$(hostname):/etc/gtk-2.0/gtkrc"

# Correct keyboard layout
if [ -f "$HOME/.Xmodmap" ]; then
	xmodmap "$HOME/.Xmodmap"
fi

# Allow root to use this display (hello sudo!)
# (Also add DISPLAY and GTK2_RC_FILES to env_keep in /etc/sudoers)
xhost +local:root

# VNC server
x11vnc -o /var/log/x11vnc.log -forever -bg -localhost -solid black -avahi -unixpw schnouki

# NVIDIA card configuration
nvidia-settings -l &

# D-Bus -- https://wiki.archlinux.org/index.php/D-Bus#Using_D-Bus_without_a_desktop_environment
[[ -z "$DBUS_SESSION_BUS_ADDRESS" ]] && eval "$(dbus-launch --sh-syntax --exit-with-session)"

# GPG agent
gnupginf="$HOME/.gnupg/gpg-agent.info"
if ( pgrep -u "${USER}" gpg-agent ); then
    eval `cat $gnupginf`
    eval `cut -d= -f1 $gnupginf | xargs echo export`
else
    eval `gpg-agent --daemon --write-env-file $gnupginf`
fi

# Screensaver
[[ $(hostname) != "odin" ]] && xscreensaver -no-splash &

# Tell Java we have a "non-reparenting window manager"
wmname LG3D

# Tell Java to anti-alias fonts
# https://wiki.archlinux.org/index.php/Java_Runtime_Environment_Fonts
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=gasp'

# Emacs daemon
[[ -S "/tmp/emacs$UID/server" ]] || (emacs --daemon &)

# Useless apps to run at boot...
dropboxd &
gnote &
gnotero &
#gajim &
#thunderbird &
#firefox &

# Be nice to my eyes
gtk-redshift -l 48.7:6.2 -t 6500:4000 &>/dev/null &

exec awesome
#exec twm
