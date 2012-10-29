local naughty, io, os, string = require("naughty"), require("io"), require("os"), require("string")
local yaml = require("yaml")

module("clistats")

local statsfile

local function fail(msg)
   naughty.notify({ preset = naughty.config.presets.critical,
                    title = "clistats error",
                    text = msg})
end

function init()
   local data_home = os.getenv("XDG_DATA_HOME")

   if data_home == nil then
      fail("XDG_DATA_HOME is not defined, can't log client focus changes")
   else
      local err
      statsfile, err = io.open(data_home .. "/awesome-clistats", "a+")
      if statsfile == nil then
         fail("Can't open stats file: " .. err)
      end
   end
end

function focus(c)
   if statsfile and c then
      local data = {
         timestamp = os.time(),
         name = c.name,
         class = c.class,
         instance = c.instance,
         role = c.role,
         type = c.type,
      }
      local msg = yaml.dump(data)
      statsfile:write(msg)
      statsfile:flush()
   end
end
