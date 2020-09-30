set -g VIRTUALFISH_VERSION 2.1.0
set -g VIRTUALFISH_PYTHON_EXEC /usr/bin/python
set -g VIRTUALFISH_ACTIVATION_FILE .workon

source /usr/lib/python3.8/site-packages/virtualfish/virtual.fish
source /usr/lib/python3.8/site-packages/virtualfish/compat_aliases.fish
source /usr/lib/python3.8/site-packages/virtualfish/auto_activation.fish
emit virtualfish_did_setup_plugins
