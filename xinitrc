#!/usr/bin/zsh

# Second screen
xrandr | grep -q "VGA1 connected" && \
    xrandr --output LVDS1 --auto --preferred \
           --output VGA1  --auto --left-of LVDS1

# Fuck Xorg -- https://bugs.freedesktop.org/show_bug.cgi?id=23705
[[ $(hostname) == "odin" || $(hostname) == "baldr" ]] && \
    xrandr --fbmm `xrandr | sed -n '/ connected / {s/.* \([0-9]\+\)mm x \([0-9]\+\)mm/\1x\2/p;q}'`

# GTK-2.0 RC files
export GTK2_RC_FILES="$HOME/.config/gtkrc-2.0:$HOME/.config/gtkrc-$(hostname):/etc/gtk-2.0/gtkrc"

# QT5 style
export QT_STYLE_OVERRIDE=gtk

# Tell Java to anti-alias fonts
# https://wiki.archlinux.org/index.php/Java#Tips_and_tricks
export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

# Source scripts in /etc/X11/xinit/xinitrc.d/
if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

systemctl --user import-environment

# From https://bbs.archlinux.org/viewtopic.php?pid=1303951#p1303951
systemctl --user start xorg.target
sleep 5
systemctl --user start applications.target
while true; do sleep 1h; done
