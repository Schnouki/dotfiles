local io, string, setmetatable = require("io"), require("string"), setmetatable
local gears, naughty, wibox = require("gears"), require("naughty"), require("wibox")
local lousy, markup = require("lousy"), require("markup")

module("locksmon")

-- {{{ Colors
local colors = {
   bad  = "#ac7373", -- red-2
   good = "#afd8af"  -- green+3
}

local function col(tag, text)
   return markup.fg.color(colors[tag], text)
end
-- }}}
-- {{ Class definition
local LocksMon = {}

function LocksMon:new()
   local t = gears.timer { timeout = 0.25 }
   local o = { widget = wibox.widget.textbox(), timer = t, key = nil,
               tries = 4,
               current_cl = lousy.get_caps_lock(),
               current_nl = lousy.get_num_lock(),
             }
   local timeout_func = function()
      o.tries = o.tries - 1
      if o.tries == 0 then
         t:stop()
      end
      o:immediateUpdate()
   end
   t:connect_signal("timeout", timeout_func)
   setmetatable(o, self)
   self.__index = self
   return o
end

function LocksMon:immediateUpdate()
   local cl = lousy.get_caps_lock()
   local nl = lousy.get_num_lock()

   local s = ""
   if cl > 0 then --cl and cl == "on" then
      s = s .. col("bad", "S")
   else
      s = s .. "_"
   end
   if nl > 0 then --nl and nl == "on" then
      s = s .. col("good", "#")
   else
      s = s .. "_"
   end
   self.widget:set_markup(s)

   if self.key == "Caps_Lock" and self.current_cl ~= cl then
      s = "Caps Lock is "
      if cl > 0 then
         s = s .. col("bad", "ON")
      else
         s = s .. col("good", "OFF")
      end
      naughty.notify({ text = markup.big(markup.bold(s)) })
   elseif self.key == "Num_Lock" and self.current_nl ~= nl then
      s = "Num Lock is "
      if nl > 0 then
         s = s .. col("good", "ON")
      else
         s = s .. col("bad", "OFF")
      end
      naughty.notify({ text = markup.big(markup.bold(s)) })
   end
   self.current_cl = cl
   self.current_nl = nl
end

function LocksMon:update(key)
   self.key = key
   self.tries = 4
   self.timer:start()
end
-- }}}

function new()
   local w = LocksMon:new()
   w:update()
   return w
end
