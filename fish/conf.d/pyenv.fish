if test -d $HOME/.pyenv
    set -x PATH "$HOME/.pyenv/bin:$PATH"
    pyenv init - fish | source
end
