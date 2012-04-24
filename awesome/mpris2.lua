local dbus, naughty = require("dbus"), require("naughty")
local io, string, tonumber = require("io"), require("string"), tonumber

local script = "~/.config/awesome/mpris2.py"
local notif_id

module("mpris2")

function init()
   dbus.add_match("session", "type='signal',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged',path='/org/mpris/MediaPlayer2'")
   dbus.add_signal("org.freedesktop.DBus.Properties",
                   function(data)
                      if data["path"] == "/org/mpris/MediaPlayer2" then
                         info()
                      end
                   end)
end

function info()
   local pipe = io.popen(script .. " info")
   local data = {}

   for line in pipe:lines() do
      for k, v in line:gmatch("(.+) => (.+)") do
         data[k] = v
      end
   end
   pipe:close()

   local title
   local text = ""
   local image
   if data["playback"] == "Stopped" then
      title = "Stopped"
   else

      local length = tonumber(data["mpris:length"]) / 1000000

      title = data["xesam:title"] or "Now playing"

      text = text .. ("by <b>" .. data["xesam:artist"] .. "</b>\n") or ""
      text = text .. ("on <i>" .. data["xesam:album"] .. "</i>\n") or ""
      text = text .. "\n"
      text = text .. data["playback"] .. " - "

      local pos = tonumber(data["position"]) / 1000000
      text = text .. string.format("%d:%02d", pos/60, pos%60)

      if data["mpris:length"] then
         local len = tonumber(data["mpris:length"]) / 1000000
         text = text .. string.format(" / %d:%02d", len/60, len%60)
      end

      if data["tracks_position"] then
         text = text .. "\nTrack " .. data["tracks_position"]
         text = text .. (" / " .. data["tracks_total"]) or ""
      end

      if data["mpris:artUrl"] then
         image = data["mpris:artUrl"]:match("file://(.+)")
      end
   end

   title = title:gsub("&", "&amp;")
   text = text:gsub("&", "&amp;")

   local notif = naughty.notify({
                                   title = title,
                                   text = text,
                                   icon = image,
                                   icon_size = 96,
                                   replaces_id = notif_id
                                })
   notif_id = notif["id"]
end

local function command(param)
   local pipe = io.popen(script .. " " .. param)
   pipe:read("*a")
   pipe:close()
end

function playpause() command("playpause") end
function stop()      command("stop")      end
function next()      command("next")      end
function prev()      command("prev")      end
