set -l cmd_path ~/.config/manage-dotfiles
set -l commands install list encrypt setup

# Disable file completions
complete -p $cmd_path -f

# All commands
complete -p $cmd_path -n "not __fish_seen_subcommand_from $commands" -a "$commands"

complete -p $cmd_path -n __fish_use_subcommand -a install -d "Install pre-commit hook"
complete -p $cmd_path -n __fish_use_subcommand -a list -d "List update files"
complete -p $cmd_path -n __fish_use_subcommand -a encrypt -d "Update encrypted files"
complete -p $cmd_path -n __fish_use_subcommand -a setup -d "Setup symlinks and decrypt encrypted files"
