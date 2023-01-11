if test "$TERM" = foot -o "$TERM" = foot-extra
    # Check that this host has the right terminfo file
    if not _has_terminfo "$TERM"
        if _has_terminfo foot-extra
            set -x TERM foot-extra
        else if _has_terminfo foot
            set -x TERM foot
        else if _has_terminfo xterm-256color
            set -x TERM xterm-256color
        else
            set -x TERM xterm
        end
    end

    # https://codeberg.org/dnkl/foot/wiki#jumping-between-prompts
    function mark_prompt_start --on-event fish_prompt
        echo -en "\e]133;A\e\\"
    end
end
