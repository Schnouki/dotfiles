#!/bin/zsh

# Setup ~/.emacs.d and ~/.emacs
setup_dir_symlink "emacs" ".emacs.d"
setup_file_symlink "emacs/init.el" ".emacs"
