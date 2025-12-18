# Generic
set -Ux XDG_CURRENT_DESKTOP sway

# GTK
# set -Ux GDK_BACKEND wayland
set -Ux GTK2_RC_FILES "$HOME/.config/gtkrc-2.0:$HOME/.config/gtkrc-(hostnamectl hostname):/etc/gtk-2.0/gtkrc"

# Qt
set -Ux QT_QPA_PLATFORM wayland
set -Ux QT_STYLE_OVERRIDE gtk

# Clutter
set -Ux CLUTTER_BACKEND wayland

# SDL2
# set -Ux SDL_VIDEODRIVER wayland

# Firefox & Thunderbird
set -Ux MOZ_ENABLE_WAYLAND 1

# Java
set -Ux _JAVA_AWT_WM_NONREPARENTING 1
