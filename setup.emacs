#!/bin/zsh

# Setup ~/.emacs.d and ~/.emacs
setup_dir_symlink "emacs" ".emacs.d"
setup_file_symlink "emacs/init.el" ".emacs"

# Fetch el-get if it's not already there
if [ ! -e emacs/el-get/el-get/el-get.el ]; then
    mkdir -p emacs/el-get \
        && git clone git://github.com/dimitri/el-get.git emacs/el-get/el-get \
        || exit 1
fi
