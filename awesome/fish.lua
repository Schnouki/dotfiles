local io, string, setmetatable = require("io"), require("string"), setmetatable
local awful, naughty, timer, wibox = require("awful"), require("naughty"), timer, require("wibox")

module("fish")

local fish = {}

function fish:new()
   local t = timer { timeout = 0.5 }
   local o = {
      widget = wibox.widget.textbox(),
      timer = t,
      state = 1,
      states = { "<°}))o»«", "<°)})o>«", "<°))}o»<" },
   }
   o.widget:buttons(
      awful.util.table.join(
         awful.button({ }, 1, function() o:fortune() end)
   ))
   t:connect_signal("timeout", function() o:update() end)
   setmetatable(o, self)
   self.__index = self
   return o
end

function fish:fortune()
    local fh = io.popen("fortune -n 100 -s")
    local fortune = fh:read("*all")
    fh:close()
    fortune = string.sub(fortune, 1, -2)
    naughty.notify({ text = fortune, timeout = 7 })
end

function fish:update()
   local t = "<span font_desc=\"mono\">"
   t = t .. awful.util.escape(self.states[self.state])
   t = t .. "</span>"
   self.widget:set_markup(t)
   self.state = (self.state + 1) % #(self.states) + 1
   self.timer:start()
end

function new()
   local w = fish:new()
   w:update()
   return w
end
