# From https://github.com/akermu/emacs-libvterm#shell-side-configuration
function vterm_printf
    if begin
            [ -n "$TMUX" ]; and string match -q -r "screen|tmux" "$TERM"
        end
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

# From https://github.com/akermu/emacs-libvterm#vterm-clear-scrollback
if [ "$INSIDE_EMACS" = vterm ]
    function clear
        vterm_printf "51;Evterm-clear-scrollback"
        tput clear
    end
end

# From https://github.com/akermu/emacs-libvterm#vterm-clear-scrollback
# function fish_title
#     hostname
#     echo ":"
#     prompt_pwd
# end

# From https://github.com/akermu/emacs-libvterm#directory-tracking-and-prompt-tracking
# function vterm_prompt_end;
#     vterm_printf '51;A'(whoami)'@'(hostnamectl hostname)':'(pwd)
# end
# functions --copy fish_prompt vterm_old_fish_prompt
# function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
#     # Remove the trailing newline from the original prompt. This is done
#     # using the string builtin from fish, but to make sure any escape codes
#     # are correctly interpreted, use %b for printf.
#     printf "%b" (string join "\n" (vterm_old_fish_prompt))
#     vterm_prompt_end
# end

# From https://github.com/akermu/emacs-libvterm#message-passing
function vterm_cmd --description 'Run an Emacs command among the ones been defined in vterm-eval-cmds.'
    set -l vterm_elisp ()
    for arg in $argv
        set -a vterm_elisp (printf '"%s" ' (string replace -a -r '([\\\\"])' '\\\\\\\\$1' $arg))
    end
    vterm_printf '51;E'(string join '' $vterm_elisp)
end
