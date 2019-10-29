#!/usr/bin/zsh

# Source scripts in /etc/X11/xinit/xinitrc.d/
if [ -d /etc/X11/xinit/xinitrc.d ]; then
    for f in /etc/X11/xinit/xinitrc.d/*; do
        [ -x "$f" ] && . "$f"
    done
    unset f
fi

# Load .Xmodmap if it exists
[[ -f ~/.Xmodmap ]] && xmodmap ~/.Xmodmap

source ~/.xsession

while true; do sleep 1h; done
