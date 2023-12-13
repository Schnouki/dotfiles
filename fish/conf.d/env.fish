# Paths
set -x fish_user_paths
fish_add_path ~/bin
fish_add_path ~/.local/bin

set -Ux XDG_DATA_HOME $HOME/.local/share
set -Ux XDG_STATE_HOME $HOME/.local/state
set -Ux XDG_CONFIG_HOME $HOME/.config
set -Ux XDG_CACHE_HOME $HOME/.cache

# Programs
set -gx EDITOR "emacsclient -c -a \"\""
set -gx BROWSER firefox

# Program-specifig options

# Gerbil
fish_add_path /opt/gerbil/bin

# Go
set -gx GOPATH $HOME/.go
fish_add_path $GOPATH/bin

# Guix
fish_add_path ~/.config/guix/current/bin

# Lua & Luarocks
set -l _lua 5.4
fish_add_path ~/.luarocks/bin
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

# Roswell
fish_add_path ~/.roswell/bin
