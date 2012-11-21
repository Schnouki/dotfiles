local io, os, pairs, setmetatable = require("io"), require("os"), pairs, setmetatable
local widget = require("widget")

module("ipmon")

-- {{{ Internals
local status_files = {}

local function last_ping_result(host)
   if status_files[host] == nil then
      return nil
   else
      local f = io.open(status_files[host])
      local val = f:read("*a")
      f:close()
      return (#val > 0)
   end
end

local function new_ping(host)
   if status_files[host] == nil then
      status_files[host] = os.tmpname()
   end
   local cmd = "fping -a " .. host .. " >" .. status_files[host] .. " 2>/dev/null &"
   os.execute(cmd)
end

local function col(color, text)
   return "<span color=\"" .. color .. "\">" .. text .. "</span>"
end
-- }}}
-- {{{ Class definition
local IPmon = {}

function IPmon:new(ips)
   local o = { widget = widget({ type = "textbox" }),
               ips = ips}
   setmetatable(o, self)
   self.__index = self
   return o
end

function IPmon:update()
   local s = ""
   for k, ip in pairs(self.ips) do
      local ip_status = last_ping_result(ip)
      new_ping(ip)
      local ip_col
      if ip_status == nil then
         ip_col = "#f0dfaf" -- yellow
      elseif ip_status then
         ip_col = "#afd8af" -- green+3
      else
         ip_col = "#ac7373" -- red-2
      end
      s = s .. col(ip_col, k)
   end
   self.widget.text = s
end
-- }}}

function new(ips)
   local w = IPmon:new(ips)
   w:update()
   return w
end
