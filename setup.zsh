#!/bin/zsh

# Setup ~/.zshenv and ~/.zshrc
setup_file_symlink "zsh/profile" ".zshenv"
setup_file_symlink "zsh/zshrc"   ".zshrc"

# Setup ~/.config/zsh/private
setup_decrypt "zsh/private.gpg" ".config/zsh/private"
