local io, pairs, print, setmetatable, string = io, pairs, print, setmetatable, string
local awful, wibox = require("awful"), require("wibox")
local deferred = require("deferred/deferred")

module("netmon")

local colors = {
   up      = "#afd8af", -- green+3,
   no_ping = "#ac7373", -- red-2
   unknown = "#f0dfaf", -- yellow
   down    = "#1E2320"  -- black
}

-- {{{ Internals
-- From http://lua-users.org/wiki/StringTrim (trim6)
local function trim(s)
  return s:match'^()%s*$' and '' or s:match'^%s*(.*%S)'
end

local function col(color, text)
   return "<span color=\"" .. color .. "\">" .. text .. "</span>"
end

local function get_interfaces(ifnames)
   local d = deferred.new()
   local status = {}
   awful.spawn.with_line_callback(
      "ip -o -4 addr show", {
         stdout = function(line)
            for k, iface in pairs(ifnames) do
               local ifname, addr = line:match("^%d+:%s+(" .. iface .. ")%s+inet%s+(%S+)")
               if ifname ~= nil then
                  status[k] = { ifname = ifname, addr = addr }
               end
            end
         end,
         exit = function(reason, code)
            if reason == "exit" and code == 0 then
               d:resolve(status)
            else
               d:reject("interfaces " .. reason .. ": " .. code)
            end
         end
   })
   return d
end

local function ping(hosts)
   local d = deferred.new()
   awful.spawn.easy_async(
      "fping -a -t 250 -r 0 " .. hosts,
      function(stdout, stderr, exitreason, exitcode)
         if exitreason == "signal" then
            d:reject("fping " .. exitreason .. ": " .. exitcode )
         else
            stdout = trim(stdout)
            d:resolve(#stdout > 0)
         end
      end
   )
   return d
end

local function format_data(ifnames, net_up, status)
   -- Build the result
   local txt = ""
   local tooltip = ""
   for k, iface in pairs(ifnames) do
      local if_status = status[k]
      local if_color
      if if_status then
         tooltip = string.format("%s<b>%s</b>: %s (<i>%s</i>)\n",
                                 tooltip, k, if_status.addr, if_status.ifname)
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
   return { text = txt, tooltip = tooltip }
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
   deferred.all({
         ping(self.hosts),
         get_interfaces(self.ifnames)
   }):next(
      function(results)
         local net_up = results[1]
         local status = results[2]

         local data = format_data(self.ifnames, net_up, status)

         self.widget:set_markup(data.text)
         self.tooltip = data.toltip
      end,
      function(err)
         print("Netmon update error: " .. err)
      end)
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
