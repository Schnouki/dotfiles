local io, string, setmetatable = require("io"), require("string"), setmetatable
local naughty, timer, wibox = require("naughty"), timer, require("wibox")
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
   local t = timer { timeout = 0.1 }
   local o = { widget = wibox.widget.textbox(), timer = t, key = nil }
   t:connect_signal("timeout",
                    function()
                       t:stop()
                       o:immediateUpdate()
                    end)
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

   if self.key == "Caps_Lock" then
      s = "Caps Lock is "
      if cl > 0 then
         s = s .. col("bad", "ON")
      else
         s = s .. col("good", "OFF")
      end
      naughty.notify({ text = markup.big(markup.bold(s)) })
   elseif self.key == "Num_Lock" then
      s = "Num Lock is "
      if nl > 0 then
         s = s .. col("good", "ON")
      else
         s = s .. col("bad", "OFF")
      end
      naughty.notify({ text = markup.big(markup.bold(s)) })
   end
end

function LocksMon:update(key)
   self.key = key
   self.timer:start()
end
-- }}}

function new()
   local w = LocksMon:new()
   w:update()
   return w
end
