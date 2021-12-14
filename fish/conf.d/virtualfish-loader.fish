set -g VIRTUALFISH_VERSION 2.5.4
set -g VIRTUALFISH_PYTHON_EXEC /usr/bin/python
set -g VIRTUALFISH_ACTIVATION_FILE .workon

source /usr/lib/python3.10/site-packages/virtualfish/virtual.fish
source /usr/lib/python3.10/site-packages/virtualfish/environment.fish
source /usr/lib/python3.10/site-packages/virtualfish/compat_aliases.fish
source /usr/lib/python3.10/site-packages/virtualfish/auto_activation.fish
emit virtualfish_did_setup_plugins
