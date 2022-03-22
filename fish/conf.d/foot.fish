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

    # https://codeberg.org/dnkl/foot/wiki#user-content-how-to-configure-my-shell-to-emit-the-osc-7-escape-sequence
    function update_cwd_osc --on-variable PWD --description 'Notify terminals when $PWD changes'
        if status --is-command-substitution || set -q INSIDE_EMACS
            return
        end
        printf \e\]7\;file://%s%s\e\\ $hostname (string escape --style=url $PWD)
    end

    update_cwd_osc # Run once since we might have inherited PWD from a parent shell
end
