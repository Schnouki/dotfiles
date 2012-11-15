local status_file
local io, os, pairs = require("io"), require("os"), pairs

module("netmon")

function init()
   status_file = os.tmpname()
end

function if_up(ifname)
   local cmd = "ip -o -4 addr show dev " .. ifname
   local pipe = io.popen(cmd)
   local val = pipe:read("*a")
   pipe:close()
   return (#val > 0)
end

function last_ping_result()
   local f = io.open(status_file)
   local val = f:read("*a")
   f:close()
   return (#val > 0)
end

function new_ping(host)
   local cmd = "fping -a " .. host .. " >" .. status_file .. " 2>/dev/null &"
   os.execute(cmd)
end

function col(color, text)
   return "<span color=\"" .. color .. "\">" .. text .. "</span>"
end

function netmon(ifnames, host)
   local s = ""
   local net_up = last_ping_result()
   for k, v in pairs(ifnames) do
      local if_status = if_up(v)
      local this_if
      if if_status then
         if net_up then
            this_if = col("#00FF00", k)
         else
            this_if = col("yellow", k)
         end
      else
         this_if = col("#1E2320", k)
      end
      s = s .. this_if
   end
   new_ping(host)
   return s
end
