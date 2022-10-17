function _hydro_prompt_pwd --on-variable PWD
    # From https://github.com/jorgebucaran/hydro/blob/main/conf.d/hydro.fish
    set -l dir_color (set_color normal)(set_color white)
    set -l sep_color (set_color normal)(set_color --dim white)
    set -l hl_color (set_color normal)(set_color --bold white)

    set -l root (command git rev-parse --show-toplevel 2>/dev/null |
        string replace --all --regex -- "^.*/" "")
    set -l _pwd (
        string replace --ignore-case -- ~ \~ $PWD |
        string replace -- "/$root/" /:/ |
        string replace --regex --all -- "(\.?[^/]{1})[^/]*/" \$1/ |
        string replace -- : "$root" |
        string replace --regex -- '([^/]+)$' "$hl_color\$1$dir_color" |
        string replace --regex --all -- '(?!^/$)/' "$sep_color/$dir_color"
    )
    set --global _hydro_pwd "$dir_color$_pwd"
end

function fish_prompt --description 'Write out the prompt'
    # Config
    set -l prefix ""
    set -l suffix " "(set_color normal)
    set -l prompt_user (set_color --bold yellow)"❯"
    set -l prompt_root (set_color --bold red)"❯"
    set -l normal_user schnouki
    set -l other_user_color (set_color --bold green)
    set -l hostname_separator (set_color --bold yellow)"@"
    set -l hostname_color (set_color --bold white)
    set -l venv_color (set_color normal)(set_color cyan)

    # Actual logic
    set -l id_part ""
    set -l prompt $prompt_user
    set -l venv ""

    # Make sure we have a pwd
    if test -z "$_hydro_pwd"
        _hydro_prompt_pwd
    end

    if test "$USER" = root
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

    echo -n $prefix$id_part$_hydro_pwd $venv$prompt$suffix
end
