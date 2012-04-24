#!/usr/bin/env python2
# -*- coding: utf-8 -*-

import dbus
import sys

import codecs
streamWriter = codecs.lookup('utf-8')[-1]
sys.stdout = streamWriter(sys.stdout)

class Mpris2(object):
    prefix = "org.mpris.MediaPlayer2."

    def __init__(self):
        self.bus = dbus.SessionBus()

    def _get_players(self):
        for n in self.bus.list_names():
            if n.startswith(Mpris2.prefix):
                yield n

    def _get_player_name(self, name):
        players = list(self._get_players())
        if len(players) == 0:
            return None
        elif name is None:
            return players[0]

        full_name = Mpris2.prefix + name
        if full_name in players:
            return full_name
        else:
            return players[0]

    def _get_player(self, name):
        name = self._get_player_name(name)
        if name is None:
            sys.exit(1)

        player = self.bus.get_object(name, "/org/mpris/MediaPlayer2")

        player_iface = dbus.Interface(player, dbus_interface=Mpris2.prefix+"Player")
        prop_iface = dbus.Interface(player, dbus_interface="org.freedesktop.DBus.Properties")

        return (player_iface, prop_iface)

    def players(self):
        for p in self._get_players():
            print p[len(Mpris2.prefix):]

    def info(self, name=None):
        _, prop = self._get_player(name)
        player_prefix = Mpris2.prefix + "Player"
        tracks_prefix = Mpris2.prefix + "TrackList"

        md = prop.Get(player_prefix, "Metadata")
        for k, v in md.iteritems():
            vv = v
            if type(v) is dbus.Array:
                vv = ", ".join(v)
            print "%s => %s" % (k, vv)

        print "playback => %s" % prop.Get(player_prefix, "PlaybackStatus")
        print "position => %d" % prop.Get(player_prefix, "Position")
        print "repeat => %s" % prop.Get(player_prefix, "LoopStatus")
        print "shuffle => %s" % prop.Get(player_prefix, "Shuffle")

        try:
            tracks = prop.Get(tracks_prefix, "Tracks")
            print "tracks_total => %d" % len(tracks)
            pos = tracks.index(md["mpris:trackid"])
            print "tracks_position => %d" % (pos+1)
        except (dbus.DBusException, ValueError):
            pass

    def playpause(self, name=None):
        self._get_player(name)[0].PlayPause()

    def stop(self, name=None):
        self._get_player(name)[0].Stop()

    def next(self, name=None):
        self._get_player(name)[0].Next()

    def prev(self, name=None):
        self._get_player(name)[0].Previous()

if __name__ == "__main__":
    args = ["players", "playpause", "stop", "info", "next", "prev"]

    if len(sys.argv) < 2 or sys.argv[1] not in args:
        print >>sys.stderr, "Usage: %s %s" % (sys.argv[0], "|".join(args))
        sys.exit(1)
    else:
        m = Mpris2()
        f = getattr(m, sys.argv[1])
        f(*sys.argv[2:])
