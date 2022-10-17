# Inspired by:
# - https://github.com/fish-shell/fish-shell/pull/8142

function __tp_reset_transient --on-event fish_postexec
    set -g __tp_transient 0
end

function __tp_prompt --on-event fish_prompt
    if test -z "$__tp_last_prompt"; and test "$__tp_transient" = 1
        set -g __tp_transient 0
    end
end


#function __tp_maybe_set_transient --on-event fish_preexec

function __tp_maybe_execute
    set -g __tp_last_prompt (commandline)
    if commandline --is-valid; or test -z "$__tp_last_prompt"
        set -g __tp_transient 1
        commandline -f repaint
    else
        set -g __tp_transient 0
    end
    commandline -f execute
end

bind \r __tp_maybe_execute
