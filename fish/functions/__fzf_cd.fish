function __fzf_cd --description "Change the current working directory."
    set fd_cmd (command -v fd || echo "fd")
    set --append fd_cmd --color=always --type=directory $fzf_fd_opts
    set fzf_arguments --multi --ansi $fzf_dir_opts $fzf_directory_opts

    set token (commandline --current-token)
    # expand any variables or leading tilde (~) in the token
    set expanded_token (eval echo -- $token)
    # unescape token because it's already quoted so backslashes will mess up the path
    set unescaped_exp_token (string unescape -- $expanded_token)

    # If the current token is a directory and has a trailing slash,
    # then use it as fd's base directory.
    if string match --quiet -- "*/" $unescaped_exp_token && test -d "$unescaped_exp_token"
        set --append fd_cmd --base-directory=$unescaped_exp_token
        # use the directory name as fzf's prompt to indicate the search is limited to that directory
        set --prepend fzf_arguments --prompt="Search Directory $unescaped_exp_token> " --preview="_fzf_preview_file $expanded_token{}"
        set file_paths_selected $unescaped_exp_token($fd_cmd 2>/dev/null | _fzf_wrapper $fzf_arguments)
    else
        set --prepend fzf_arguments --prompt="Search Directory> " --query="$unescaped_exp_token" --preview='_fzf_preview_file {}'
        set file_paths_selected ($fd_cmd 2>/dev/null | _fzf_wrapper $fzf_arguments)
    end

    if test $status -eq 0
        # set --prepend file_paths_selected cd
        # commandline --replace -- (string escape -- $file_paths_selected | string join ' ')
        cd -- (string escape -- $file_paths_selected | string join ' ')
        commandline --replace ''
    end

    commandline --function repaint
end
