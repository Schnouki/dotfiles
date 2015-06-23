local io, string, setmetatable = require("io"), require("string"), setmetatable
local awful, naughty, wibox = require("awful"), require("naughty"), require("wibox")

module("fish")

local fish = {}

function fish:new()
   local cd = awful.util.getdir("config")
   local o = {
      widget = wibox.widget.imagebox(),
      icon_small = cd .. "/icons/fortune-cookie-16.png",
      icon_big = cd .. "/icons/fortune-cookie-32.png",
      timeout = 7
   }
   o.widget:set_image(o.icon_small)
   o.widget:buttons(
      awful.util.table.join(
         awful.button({ }, 1, function() o:fortune() end),
         awful.button({ }, 3, function() o:fortune("fr") end)
   ))
   setmetatable(o, self)
   self.__index = self
   return o
end

function fish:fortune(lang)
   if lang == nil then
      lang = ""
   end
   local fh = io.popen("fortune -ac " .. lang)
   local cookie = fh:read("*line")
   local fortune = fh:read("*all")
   fh:close()
   cookie = string.sub(cookie, 2, -2)
   fortune = string.sub(fortune, 2, -2)
   naughty.notify({
         title = "Fortune about " .. cookie,
         text = fortune,
         timeout = self.timeout,
         icon = self.icon_big
   })
end

function new()
   return fish:new()
end
