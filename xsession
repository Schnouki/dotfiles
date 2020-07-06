#!/bin/sh

if [[ $(hostname) == "baldr" ]]; then
    # Second screen
    xrandr | grep -q "VGA1 connected" && \
        xrandr --output LVDS1 --auto --preferred \
               --output VGA1  --auto --left-of LVDS1

    # Fuck Xorg -- https://bugs.freedesktop.org/show_bug.cgi?id=23705
    [[ $(hostname) == "baldr" ]] && \
        xrandr --dpi 118
fi

if [[ $(hostname) == "loki" ]]; then
    _ffx_dir="$HOME/.mozilla/firefox/u97j7qi0.default-release"
    _screen=""
    if xrandr | egrep '^DP1 connected' >/dev/null; then
        _screen="DP1"
    elif xrandr | egrep '^DP3 connected' >/dev/null; then
        _screen="DP3"
    fi
    if [[ -z "$_screen" ]]; then
        # 1 screen
        xrandr --dpi 288
        echo 'Xft.dpi: 288' | xrdb -merge
        sed -i 's/user_pref("layout.css.devPixelsPerPx".*/user_pref("layout.css.devPixelsPerPx", "2");/' \
            "$_ffx_dir/user.js"
    else
        # 2 screens
        xrandr --dpi 120 --output eDP1 --mode 1920x1080 --output $_screen --primary --auto --right-of eDP1
        echo 'Xft.dpi: 120' | xrdb -merge
        sed -i 's/user_pref("layout.css.devPixelsPerPx".*/user_pref("layout.css.devPixelsPerPx", "1");/' \
            "$_ffx_dir/user.js"
    fi
    export QT_AUTO_SCREEN_SCALE_FACTOR=1
fi

# GTK-2.0 RC files
export GTK2_RC_FILES="$HOME/.config/gtkrc-2.0:$HOME/.config/gtkrc-$(hostname):/etc/gtk-2.0/gtkrc"

# QT5 style
export QT_STYLE_OVERRIDE=gtk

# Tell Java to anti-alias fonts
# https://wiki.archlinux.org/index.php/Java#Tips_and_tricks
#export _JAVA_OPTIONS='-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'

systemctl --user import-environment

# From https://bbs.archlinux.org/viewtopic.php?pid=1303951#p1303951
systemctl --user start xorg.target
systemctl --user start applications.target &
