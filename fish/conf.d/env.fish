# Paths
set PATH \
    $HOME/bin \
    $HOME/.local/bin \
    $HOME/.cabal/bin \
    $HOME/.composer/vendor/bin \
    $HOME/.gem/ruby/2.5.0/bin \
    $HOME/.go/bin \
    $HOME/.local/share/jpm_tree/bin \
    $HOME/.local/share/nimble/bin \
    $HOME/.luarocks/bin \
    $HOME/.roswell/bin \
    $HOME/.config/guix/current/bin \
    /opt/gerbil/bin \
    $PATH

set -Ux XDG_DATA_HOME $HOME/.local/share
set -Ux XDG_STATE_HOME $HOME/.local/state
set -Ux XDG_CONFIG_HOME $HOME/.config
set -Ux XDG_CACHE_HOME $HOME/.cache

# Programs
set -gx EDITOR "emacsclient -c -a \"\""
set -gx BROWSER firefox

# Program-specifig options

# Go
set -gx GOPATH $HOME/.go

# Lua & Luarocks
set -l _lua 5.4
set -gx LUA_PATH (string join ";" \
    /usr/share/lua/$_lua/\?.lua \
    /usr/share/lua/$_lua/\?/init.lua \
    /usr/lib/lua/$_lua/\?.lua \
    /usr/lib/lua/$_lua/\?/init.lua \
    ./\?.lua \
    ./\?/init.lua \
    $HOME/.luarocks/share/lua/$_lua/\?.lua \
    $HOME/.luarocks/share/lua/$_lua/\?/init.lua \
)
set -gx LUA_CPATH (string join ";" \
    /usr/lib/lua/$_lua/\?.so \
    /usr/lib/lua/$_lua/loadall.so \
    ./\?.so \
    $HOME/.luarocks/lib/lua/$_lua/\?.so \
)

# Python
set -gx PYTHONSTARTUP $HOME/.config/python_startup.py

# Ripgrep
set -gx RIPGREP_CONFIG_PATH $HOME/.config/ripgreprc
