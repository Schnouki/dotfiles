source /usr/share/fish/vendor_completions.d/pueue.fish
complete -c pueue -n "__fish_seen_subcommand_from add" -d 'The command to be added' -x -a '(__fish_complete_subcommand --fcs-skip=2)'
