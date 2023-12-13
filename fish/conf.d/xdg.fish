# Changes suggested by xdg-ninja

# AWS
set -Ux AWS_SHARED_CREDENTIALS_FILE "$XDG_CONFIG_HOME"/aws/credentials
set -Ux AWS_CONFIG_FILE "$XDG_CONFIG_HOME"/aws/config

# Cargo
set -Ux CARGO_HOME "$XDG_DATA_HOME"/cargo

# CUDA
set -Ux CUDA_CACHE_PATH "$XDG_CACHE_HOME"/nv

# Docker
set -Ux DOCKER_CONFIG "$XDG_CONFIG_HOME"/docker

# Janet
set -Ux JANET_TREE "$XDG_DATA_HOME"/jpm_tree
#set -Ux JANET_PATH "$XDG_DATA_HOME"/jpm_tree/lib
fish_add_path $JANET_TREE/bin

# Less
set -Ux LESSHISTFILE "$XDG_DATA_HOME"/less_history

# mitmproxy
alias mitmproxy="mitmproxy --set confdir=$XDG_CONFIG_HOME/mitmproxy"
alias mitmweb="mitmweb --set confdir=$XDG_CONFIG_HOME/mitmproxy"

# mypy
set -Ux MYPY_CACHE_DIR "$XDG_CACHE_HOME"/mypy

# MySQL
set -Ux MYSQL_HISTFILE "$XDG_DATA_HOME"/mysql_history

# Nimble
set -Ux NIMBLE_DIR "$XDG_DATA_HOME"/nimble
fish_add_path $NIMBLE_DIR/bin

# NodeJS
set -Ux NODE_REPL_HISTORY "$XDG_DATA_HOME"/node_repl_history

# PostgreSQL
set -Ux PSQLRC "$XDG_CONFIG_HOME/pg/psqlrc"

# Pylint
set -Ux PYLINTHOME "$XDG_CACHE_HOME"/pylint

# Readline
set -Ux INPUTRC "$XDG_CONFIG_HOME"/inputrc

# SQLite
set -Ux SQLITE_HISTORY "$XDG_DATA_HOME"/sqlite_history

# Units
alias units="units --history $XDG_DATA_HOME/units_history"

# Virtualenv
set -Ux WORKON_HOME "$XDG_DATA_HOME"/virtualenvs

# Wget
alias wget="wget --hsts-file=$XDG_DATA_HOME/wget-hsts"

# Wine
set -Ux WINEPREFIX "$XDG_DATA_HOME"/wine

# Xorg-xauth
set -Ux XAUTHORITY "$XDG_RUNTIME_DIR"/Xauthority
