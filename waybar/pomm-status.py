#!/usr/bin/env python

import json
import sys
import gi
import re

gi.require_version("Gtk", "3.0")
from gi.repository import GLib, Gio

# D-Bus configuration
SERVICE_NAME = "org.gnu.Emacs"
OBJECT_PATH = "/net/schnouki/pomm"
INTERFACE_NAME = "net.schnouki.pomm"
PROPERTY_NAME = "mode-line"


def on_properties_changed(
    connection,
    sender_name,
    object_path,
    interface_name,
    signal_name,
    parameters,
    user_data,
):
    # Extract the changed properties
    changed_properties = parameters[1]

    # Check if our property of interest has changed
    if PROPERTY_NAME in changed_properties:
        mode_line = changed_properties[PROPERTY_NAME]
        output_waybar(mode_line)


def parse_mode_line(mode_line):
    """Parse the mode line to extract class information and content"""
    if mode_line is None:
        return "", "", ""

    # Strip leading and trailing whitespaces
    mode_line = mode_line.strip()

    # Check if it starts with [keyword]
    match = re.match(r"^\[([\w-]+)\](.*)", mode_line)
    if match:
        keyword = match.group(1)
        content = match.group(2).strip()
        return keyword, content, mode_line

    return "", "", mode_line


def output_waybar(mode_line):
    """Format and output the mode-line for Waybar"""
    css_class, content, full_text = parse_mode_line(mode_line)

    output = {"text": content, "tooltip": full_text, "class": css_class}
    print(json.dumps(output), flush=True)


def get_property_value():
    """Get the initial property value"""
    try:
        # Create a D-Bus proxy
        bus = Gio.bus_get_sync(Gio.BusType.SESSION, None)
        proxy = Gio.DBusProxy.new_sync(
            bus,
            Gio.DBusProxyFlags.NONE,
            None,
            SERVICE_NAME,
            OBJECT_PATH,
            "org.freedesktop.DBus.Properties",
            None,
        )

        # Get the property value
        result = proxy.call_sync(
            "Get",
            GLib.Variant("(ss)", (INTERFACE_NAME, PROPERTY_NAME)),
            Gio.DBusCallFlags.NONE,
            -1,
            None,
        )

        # The result is a tuple with a single variant, which contains our value
        value = result.unpack()[0]
        return value
    except Exception as e:
        error_msg = str(e)
        print(
            json.dumps({"text": "[error]", "tooltip": error_msg, "class": "error"}),
            flush=True,
        )
        return None


def main():
    # Get the initial property value and output it
    mode_line = get_property_value()
    output_waybar(mode_line)

    # Set up a connection to listen for property changes
    bus = Gio.bus_get_sync(Gio.BusType.SESSION, None)

    # Subscribe to PropertiesChanged signal
    bus.signal_subscribe(
        SERVICE_NAME,
        "org.freedesktop.DBus.Properties",
        "PropertiesChanged",
        OBJECT_PATH,
        None,
        Gio.DBusSignalFlags.NONE,
        on_properties_changed,
        None,
    )

    # Start the main loop
    loop = GLib.MainLoop()
    try:
        loop.run()
    except KeyboardInterrupt:
        pass


if __name__ == "__main__":
    main()
