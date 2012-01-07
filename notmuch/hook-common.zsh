# Exit if any command fails
set -e

# Check the DISPLAY environment variable (needed for D-Bus and awesome-client)
[[ -z "$DISPLAY" ]] && export DISPLAY=:0.0 || true

CONFIGDIR=$HOME/.config/notmuch
MAILDIR=$HOME/mail

###########
# awesome #
###########
function awesome_begin_update() {
    local n
    n=$(notmuch count tag:unread)
    echo "tb_mails_updating(true)\ntb_mails_set_count($n)" | awesome-client
}
function awesome_end_update() {
    local n
    n=$(notmuch count tag:unread)
    echo "tb_mails_updating(nil)\ntb_mails_set_count($n)" | awesome-client
}
