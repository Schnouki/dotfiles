#!/bin/bash

# Check for ~/.emacs.d
if [ ! -e ~/.emacs.d ]; then
    ln -s ~/.config/emacs ~/.emacs.d
elif [ -L ~/.emacs.d ]; then
    # Correct target?
    if [ "$(readlink ~/.emacs.d)" != "$HOME/.config/emacs" ]; then
        echo "~/.emacs.d points to a wrong target :("
        exit 1
    fi
elif [ -d ~/.emacs.d ]; then
    if [ "$(/bin/ls -A ~/.emacs.d)" ]; then
        # Not empty: fail
        echo "~/.emacs.d exists and is not empty :("
        exit 1
    else
        rmdir ~/.emacs.d && ln -s ~/.config/emacs ~/.emacs.d
    fi
else
    echo "There is something wrong with ~/.emacs.d :/"
    exit 1
fi

# Check for ~/.emacs
if [ ! -e ~/.emacs ]; then
    ln -s ~/.config/emacs/init.el ~/.emacs
elif [ -L ~/.emacs ]; then
    # Correct target?
    if [ "$(readlink ~/.emacs)" != "$HOME/.config/emacs/init.el" ]; then
        echo "~/.emacs points to a wrong target :("
        exit 1
    fi
else
    echo "There is something wrong with ~/.emacs :/"
    exit 1
fi
