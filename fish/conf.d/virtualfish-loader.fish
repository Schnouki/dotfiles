set -g VIRTUALFISH_VERSION 2.5.4
set -g VIRTUALFISH_PYTHON_EXEC /usr/bin/python
set -g VIRTUALFISH_ACTIVATION_FILE .workon

set -l _vf_py_ver ($VIRTUALFISH_PYTHON_EXEC -c 'import sys; print(f"{sys.version_info.major}.{sys.version_info.minor}")')
set -l _vf_path "/usr/lib/python$_vf_py_ver/site-packages/virtualfish"

if test -e "$HOME/.local/lib/python$_vf_py_ver/site-packages/virtualfish/virtual.fish"
    set _vf_path "$HOME/.local/lib/python$_vf_py_ver/site-packages/virtualfish"
end

source "$_vf_path/virtual.fish"
source "$_vf_path/environment.fish"
source "$_vf_path/compat_aliases.fish"
source "$_vf_path/auto_activation.fish"
emit virtualfish_did_setup_plugins
