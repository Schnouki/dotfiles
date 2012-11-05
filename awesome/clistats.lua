local naughty, io, os, string = require("naughty"), require("io"), require("os"), require("string")
local yaml = require("yaml")

module("clistats")

local last_action, statsfile

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

local function add_entry(action, data)
   if statsfile and data then
      data.action = action
      data.timestamp = os.time()
      local msg = yaml.dump(data)
      statsfile:write(msg)
      statsfile:flush()
      last_action = action
   end
end

function focus(c)
   if c then
      local data = {
         action = action,
         name = c.name,
         class = c.class,
         instance = c.instance,
         role = c.role,
         type = c.type,
      }
      add_entry("focus", data)
   end
end

function idle(ms)
   if ms >= 5*1000*60 and last_action ~= "idle" then
      local data = {
         idle_time = ms,
      }
      add_entry("idle", data)
   end
end
