#!/bin/bash

# Checking for ~/.zshrc
if [ ! -e ~/.zshrc ]; then
    ln -s ~/.config/zsh/zshrc ~/.zshrc
elif [ -L ~/.zshrc ]; then
    if [ "$(readlink ~/.zshrc)" != "$HOME/.config/zsh/zshrc" ]; then
        echo "~/.zshrc points to the wrong target"
        exit 1
    fi
else
    echo "There is something wrong with ~/.zshrc :/"
    exit 1
fi
