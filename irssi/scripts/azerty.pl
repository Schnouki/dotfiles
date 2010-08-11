#!/usr/bin/perl -w

## Bugreports and Licence disclaimer.
#
# For bugreports and other improvements contact Thomas Jost <schnouki@schnouki.net>
#
#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.
#
#    You should have received a copy of the GNU General Public License
#    along with this script; if not, write to the Free Software
#    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
#
##

use strict;
use Irssi;
use vars qw($VERSION %IRSSI);

$VERSION = "0.01";

%IRSSI = (
    authors     => 'Thomas Jost',
    contact     => 'schnouki@schnouki.net',
    name        => 'azerty',
    description => 'This script will set the proper keybindings on /AZERTY.',
    license     => 'GNU GPLv2 or later',
    url         => 'http://github.com/Schnouki/dotfiles/blob/master/irssi/scripts/azerty.pl',
);

Irssi::theme_register([
    'loaded', '%R>>%n %_Scriptinfo:%_ Loaded $0 version $1 by $2.',
    'bound',  '%R>>%n %_Keybindings:%_ Loaded the $0 keybindings.'
]);

sub azerty {
    # Specify BOTH UTF-8 and ISO-8859-1 codes... So be careful when saving this
    # file, always use ISO-8859-1 and NOT UTF-8!
    Irssi::command("^BIND meta-& change_window 1");
    Irssi::command("^BIND meta-é change_window 2");
    Irssi::command("^BIND meta-Ã© change_window 2");
    Irssi::command("^BIND meta-\" change_window 3");
    Irssi::command("^BIND meta-' change_window 4");
    Irssi::command("^BIND meta-( change_window 5");
    Irssi::command("^BIND meta-- change_window 6");
    Irssi::command("^BIND meta-è change_window 7");
    Irssi::command("^BIND meta-Ã¨ change_window 7");
    Irssi::command("^BIND meta-_ change_window 8");
    Irssi::command("^BIND meta-ç change_window 9");
    Irssi::command("^BIND meta-Ã§ change_window 9");
    Irssi::command("^BIND meta-à change_window 10");
    Irssi::command("^BIND meta-Ã  change_window 10");

    Irssi::command("^BIND meta-a change_window 11");
    Irssi::command("^BIND meta-z change_window 12");

    Irssi::command("^BIND -delete meta-q");
    Irssi::command("^BIND -delete meta-w");

    Irssi::printformat(MSGLEVEL_CLIENTCRAP, 'bound', 'azerty');
}

Irssi::command_bind('azerty', 'azerty');

Irssi::settings_add_bool($IRSSI{name}, 'azerty_set_on_load', 0);
if (Irssi::settings_get_bool('azerty_set_on_load')) {
    Irssi::command('^azerty');
}

Irssi::printformat(MSGLEVEL_CLIENTCRAP, 'loaded', $IRSSI{name}, $VERSION, $IRSSI{authors});

