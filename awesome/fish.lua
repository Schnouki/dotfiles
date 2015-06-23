local io, string, setmetatable = require("io"), require("string"), setmetatable
local awful, naughty, wibox = require("awful"), require("naughty"), require("wibox")

module("fish")

local fish = {}

function fish:new()
   local w = wibox.widget.imagebox()
   w:set_image(awful.util.getdir("config") .. "/icons/fortune-cookie.png")
   w:buttons(
      awful.util.table.join(
         awful.button({ }, 1, function() w:fortune() end),
         awful.button({ }, 3, function() w:fortune("fr") end)
   ))
   setmetatable(w, self)
   self.__index = self
   return w
end

function fish:fortune(lang)
   if lang == nil then
      lang = ""
   end
   local fh = io.popen("fortune -a " .. lang)
   local fortune = fh:read("*all")
   fh:close()
   fortune = string.sub(fortune, 1, -2)
   naughty.notify({ text = fortune, timeout = 7 })
end

function new()
   return fish:new()
end
