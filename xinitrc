#!/bin/zsh

# Fuck Xorg -- https://bugs.freedesktop.org/show_bug.cgi?id=23705
[[ $(hostname) == "odin" ]] && xrandr --fbmm `xrandr | sed -n '/ connected / {s/.* \([0-9]\+\)mm x \([0-9]\+\)mm/\1x\2/p;q}'`

# GTK-2.0 RC files
export GTK2_RC_FILES="$HOME/.config/gtkrc-2.0:$HOME/.config/gtkrc-$(hostname):/etc/gtk-2.0/gtkrc"

# Tell Java to anti-alias fonts
# https://wiki.archlinux.org/index.php/Java#Tips_and_tricks
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

# VNC server
x11vnc -o /var/log/x11vnc.log -forever -bg -localhost -solid black -avahi -unixpw schnouki

# GPG agent
gnupginf="$HOME/.gnupg/gpg-agent.info"
if ( pgrep -u "${USER}" gpg-agent ); then
    eval `cat $gnupginf`
    eval `cut -d= -f1 $gnupginf | xargs echo export`
else
    eval `gpg-agent --daemon --write-env-file $gnupginf`
fi

# Source scripts in /etc/X11/xinit/xinitrc.d/
if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

exec /bin/systemd --user
