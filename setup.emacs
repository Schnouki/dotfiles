#!/bin/zsh

# Setup ~/.emacs.d and ~/.emacs
setup_dir_symlink "emacs" ".emacs.d"
setup_file_symlink "emacs/init.el" ".emacs"

# Decrypt init-99-private.el
setup_decrypt "emacs/init-99-private.el.gpg" ".config/emacs/init-99-private.el"
