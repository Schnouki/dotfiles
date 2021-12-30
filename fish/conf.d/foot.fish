# https://codeberg.org/dnkl/foot/wiki#user-content-how-to-configure-my-shell-to-emit-the-osc-7-escape-sequence
if test "$TERM" = foot -o "$TERM" = foot-extra
    function update_cwd_osc --on-variable PWD --description 'Notify terminals when $PWD changes'
        if status --is-command-substitution || set -q INSIDE_EMACS
            return
        end
        printf \e\]7\;file://%s%s\e\\ $hostname (string escape --style=url $PWD)
    end

    update_cwd_osc # Run once since we might have inherited PWD from a parent shell
end
