function fish_right_prompt
    set -l last_status $status
    set -l last_duration $CMD_DURATION

    # Config
    set -l prefix ""
    set -l suffix " "(set_color normal)
    set -l sep (set_color normal)" / "
    set -l time_color (set_color --bold cyan)
    set -l cmd_fail_color (set_color --bold red)
    set -l vcs_color (set_color magenta)
    set -l commit_color (set_color blue)
    set -l git_staged (set_color magenta)"^"
    set -l git_unstaged (set_color magenta)"*"

    # Actual logic
    set -l now (date +%X)
    set -l prompt $time_color$now

    # Git
    if git_is_repo
        set -l git_prompt $vcs_color"git:"$commit_color(git_branch_name)
        if git_is_dirty
            set git_prompt $git_prompt$git_unstaged
        end
        if git_is_staged
            set git_prompt $git_prompt$git_staged
        end
        set prompt $git_prompt$sep$prompt
    end

    # Command status
    if test $last_status -ne 0
        set prompt $cmd_fail_color$last_status$sep$prompt
    end

    echo -n $prefix$prompt$suffix
end
