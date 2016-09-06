local io, os, pairs, setmetatable = require("io"), require("os"), pairs, setmetatable
local wibox = require("wibox")

module("netmon")

local colors = {
   up      = "#afd8af", -- green+3,
   no_ping = "#ac7373", -- red-2
   unknown = "#f0dfaf", -- yellow
   down    = "#1E2320"
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

local function if_up(ifname)
   local cmd = "ip -o -4 addr show dev " .. ifname
   local pipe = io.popen(cmd)
   local val = pipe:read("*a")
   pipe:close()
   return (#val > 0)
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
   local o = { widget = wibox.widget.textbox(),
               ifnames = ifnames,
               hosts = hosts }
   setmetatable(o, self)
   self.__index = self
   return o
end

function NetMon:update()
   local s = ""
   for k, iface in pairs(self.ifnames) do
      local if_status = if_up(iface)
      local if_color
      if if_status then
         local net_up = last_ping_result(self.hosts)
         new_ping(self.hosts)
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
      s = s .. col(colors[if_color], k)
   end
   self.widget:set_markup(s)
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
