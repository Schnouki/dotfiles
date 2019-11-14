function fish_prompt --description 'Write out the prompt'

    # Config
    set -l prefix ""
    set -l suffix " "(set_color normal)
    set -l prompt_user (set_color --bold yellow)"❯"
    set -l prompt_root (set_color --bold red)"❯"
    set -l normal_user "schnouki"
    set -l other_user_color (set_color --bold green)
    set -l hostname_separator (set_color --bold yellow)"@"
    set -l hostname_color (set_color --bold white)
    set -l dir_color (set_color --bold white)
    set -l venv_color (set_color normal)(set_color cyan)

    # Actual logic
    set -l id_part ""
    set -l prompt $prompt_user
    set -l venv ""

    if test "$USER" = "root"
        # If root, use a different prompt
        set prompt $prompt_root
    else if test "$USER" != "$normal_user"
        # If not root nor "me", show the username
        set id_part "$other_user_color$USER"
    end

    # In a remote session (SSH, tmux, screen…), show hostname
    if test -z $XDG_SEAT
        if test -n $id_part
            set id_part "$id_part$hostname_separator"
        end
        set id_part "$id_part$hostname_color"(hostname)
    end
    if test -n $id_part
        set id_part $id_part" "
    end

    # Virtualenv using VirtualFish
    if set -q VIRTUAL_ENV
        set venv $venv_color"("(basename "$VIRTUAL_ENV")") "
    end

    echo -n $prefix$id_part$dir_color(prompt_pwd) $venv$prompt$suffix
end
