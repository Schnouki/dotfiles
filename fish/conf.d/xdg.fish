# Changes suggested by xdg-ninja

# AWS
set -gx AWS_SHARED_CREDENTIALS_FILE "$XDG_CONFIG_HOME"/aws/credentials
set -gx AWS_CONFIG_FILE "$XDG_CONFIG_HOME"/aws/config

# Cargo
set -gx CARGO_HOME "$XDG_DATA_HOME"/cargo

# CUDA
set -gx CUDA_CACHE_PATH "$XDG_CACHE_HOME"/nv

# Docker
set -gx DOCKER_CONFIG "$XDG_CONFIG_HOME"/docker

# Less
set -gx LESSHISTFILE "$XDG_DATA_HOME"/less_history

# mitmproxy
alias mitmproxy="mitmproxy --set confdir=$XDG_CONFIG_HOME/mitmproxy"
alias mitmweb="mitmweb --set confdir=$XDG_CONFIG_HOME/mitmproxy"

# mypy
set -gx MYPY_CACHE_DIR "$XDG_CACHE_HOME"/mypy

# MySQL
set -gx MYSQL_HISTFILE "$XDG_DATA_HOME"/mysql_history

# Nimble
set -gx NIMBLE_DIR "$XDG_DATA_HOME"/nimble

# NodeJS
set -gx NODE_REPL_HISTORY "$XDG_DATA_HOME"/node_repl_history

# PostgreSQL
set -gx PSQLRC "$XDG_CONFIG_HOME/pg/psqlrc"

# Pylint
set -gx PYLINTHOME "$XDG_CACHE_HOME"/pylint

# Readline
set -gx INPUTRC "$XDG_CONFIG_HOME"/inputrc

# SQLite
set -gx SQLITE_HISTORY "$XDG_DATA_HOME"/sqlite_history

# Units
alias units="units --history $XDG_DATA_HOME/units_history"

# Wget
alias wget="wget --hsts-file=$XDG_DATA_HOME/wget-hsts"

# Wine
set -gx WINEPREFIX "$XDG_DATA_HOME"/wine

# Xorg-xauth
set -gx XAUTHORITY "$XDG_RUNTIME_DIR"/Xauthority
