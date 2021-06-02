if test -d $PYENV_ROOT
    set -x PATH \
        $PYENV_ROOT/bin \
        $PYENV_ROOT/shims \
        $PATH
    pyenv init - --no-rehash fish | source
end
