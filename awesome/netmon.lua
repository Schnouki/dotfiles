local io, os, pairs, setmetatable, string = io, os, pairs, setmetatable, string
local awful, wibox = require("awful"), require("wibox")
local print = print

module("netmon")

local colors = {
   up      = "#afd8af", -- green+3,
   no_ping = "#ac7373", -- red-2
   unknown = "#f0dfaf", -- yellow
   down    = "#1E2320"  -- black
}

-- {{{ Internals
local status_files = {}

local function last_ping_result(hosts)
   if status_files[hosts] == nil then
      return nil
   else
      local f = io.open(status_files[hosts])
      local val = f:read("*a")
      f:close()
      return (#val > 0)
   end
end

local function new_ping(hosts)
   if status_files[hosts] == nil then
      status_files[hosts] = os.tmpname()
   end
   local cmd = "( fping -a -t 250 -r 0 " .. hosts .. " 2>/dev/null | sponge " .. status_files[hosts] .. " ) &"
   os.execute(cmd)
end

local function col(color, text)
   return "<span color=\"" .. color .. "\">" .. text .. "</span>"
end

-- From http://lua-users.org/wiki/StringTrim (trim6)
local function trim(s)
  return s:match'^()%s*$' and '' or s:match'^%s*(.*%S)'
end
-- }}}
-- {{{ Class definition
local NetMon = {}

function NetMon:new(ifnames, hosts)
   local o = {
      widget = wibox.widget.textbox(),
      tooltip = "",
      ifnames = ifnames,
      hosts = hosts
   }
   o.widget_tooltip = awful.tooltip({
         objects = { o.widget },
         timer_function = function() return o.tooltip end
   })
   setmetatable(o, self)
   self.__index = self
   return o
end

function NetMon:update()
   local status = {}

   -- Start new pings
   new_ping(self.hosts)

   -- Get a list of interfaces
   local pipe = io.popen("ip -o -4 addr show")
   for line in pipe:lines() do
      for k, iface in pairs(self.ifnames) do
         local ifname, addr = line:match("^%d+:%s+(" .. iface .. ")%s+inet%s+(%S+)")
         if ifname ~= nil then
            status[k] = { ifname = ifname, addr = addr }
         end
      end
   end
   pipe:close()

   -- Now the ping results!
   local net_up = last_ping_result(self.hosts)

   -- Build the result
   local txt = ""
   local tooltip = ""
   for k, iface in pairs(self.ifnames) do
      local if_status = status[k]
      local if_color
      if if_status then
         tooltip = string.format("%s<b>%s</b>: %s (<i>%s</i>)\n",
                                 tooltip, k, if_status.addr, if_status.ifname)
         print(tooltip)
         if net_up == nil then
            if_color = "unknown"
         elseif net_up then
            if_color = "up"
         else
            if_color = "no_ping"
         end
      else
         if_color = "down"
      end
      txt = txt .. col(colors[if_color], k)
   end

   self.widget:set_markup(txt)
   self.tooltip = toltip
end

function NetMon:ssid(ifname)
   local iface = self.ifnames[ifname]
   if iface ~= nil then
      local cmd = "iwgetid --raw " .. iface
      local pipe = io.popen(cmd)
      local val = pipe:read("*a")
      pipe:close()
      return trim(val)
   end
end
-- }}}

function new(ifnames, hosts)
   local w = NetMon:new(ifnames, hosts)
   w:update()
   return w
end
