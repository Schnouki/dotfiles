local io, string, setmetatable = require("io"), require("string"), setmetatable
local timer, wibox = timer, require("wibox")

module("locksmon")

-- {{{ Colors
local colors = {
   caps = "#ac7373", -- red-2
   num  = "#afd8af"  -- green+3
}

local function col(tag, text)
   return "<span color=\"" .. colors[tag] .. "\">" .. text .. "</span>"
end
-- }}}
-- {{{ Locks reading
local function read_status()
   local p = io.popen("xset q")
   local val = p:read("*a")
   p:close()

   local cl = string.match(val, "Caps Lock:%s+(%w+)")
   local nl = string.match(val, "Num Lock:%s+(%w+)")

   local s = ""
   if cl and cl == "on" then
      s = s .. col("caps", "S")
   else
      s = s .. "_"
   end
   if nl and nl == "on" then
      s = s .. col("num", "#")
   else
      s = s .. "_"
   end
   return s
end
-- }}
-- {{ Class definition
local LocksMon = {}

function LocksMon:new()
   local t = timer { timeout = 0.1 }
   local o = { widget = wibox.widget.textbox(), timer = t }
   t:connect_signal("timeout",
                    function()
                       t:stop()
                       o.widget:set_markup(read_status())
                    end)
   setmetatable(o, self)
   self.__index = self
   return o
end

function LocksMon:update()
   self.timer:start()
end
-- }}}

function new()
   local w = LocksMon:new()
   w:update()
   return w
end
