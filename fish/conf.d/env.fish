# Paths
set PATH \
    $HOME/bin \
    $HOME/.local/bin \
    $HOME/.cabal/bin \
    $HOME/.go/bin \
    $HOME/.gem/ruby/2.5.0/bin \
    $HOME/.composer/vendor/bin \
    $HOME/.nimble/bin \
    $PATH

set -gx XDG_DATA_HOME $HOME/.local/share
set -gx XDG_CONFIG_HOME $HOME/.config
set -gx XDG_CACHE_HOME $HOME/.cache

# Programs
set -gx EDITOR "emacsclient -c -a \"\""
set -gx BROWSER firefox

# Program-specifig options

# Go
set -gx GOPATH $HOME/.go

# Python
set -gx PYTHONSTARTUP $HOME/.config/python_startup.py

# Ripgrep
set -gx RIPGREP_CONFIG_PATH $HOME/.config/ripgreprc
