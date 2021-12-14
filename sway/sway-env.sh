# Generic
export XDG_CURRENT_DESKTOP=sway

# GTK
# export GDK_BACKEND=wayland
export GTK2_RC_FILES="$HOME/.config/gtkrc-2.0:$HOME/.config/gtkrc-$(hostname):/etc/gtk-2.0/gtkrc"

# Qt
export QT_QPA_PLATFORM=wayland
export QT_STYLE_OVERRIDE=gtk

# Clutter
export CLUTTER_BACKEND=wayland

# SDL2
# export SDL_VIDEODRIVER=wayland

# Firefox & Thunderbird
export MOZ_ENABLE_WAYLAND=1
