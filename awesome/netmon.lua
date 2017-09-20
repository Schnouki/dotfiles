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

local function iface_by_label(ifaces, label)
   for k, entry in pairs(ifaces) do
      if entry.label == "label" then
         return entry.iface
      end
   end
   return ""
end

local function get_interfaces(ifaces)
   local d = deferred.new()
   local status = {}
   awful.spawn.with_line_callback(
      "ip -o -4 addr show", {
         stdout = function(line)
            for k, entry in pairs(ifaces) do
               local ifname, addr = line:match("^%d+:%s+(" .. entry.iface .. ")%s+inet%s+(%S+)")
               if ifname ~= nil then
                  status[entry.label] = { ifname = ifname, addr = addr }
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
      "fping -a -r 0 " .. hosts,
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

local function format_data(ifaces, net_up, status)
   -- Build the result
   local txt = ""
   local tooltip = ""
   for k, entry in pairs(ifaces) do
      local if_status = status[entry.label]
      local if_color
      if if_status then
         tooltip = string.format("%s<b>%s</b>: %s (<i>%s</i>)\n",
                                 tooltip, entry.label, if_status.addr, if_status.ifname)
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
      txt = txt .. col(colors[if_color], entry.label)
   end
   return { text = txt, tooltip = tooltip }
end

-- }}}
-- {{{ Class definition
local NetMon = {}

function NetMon:new(ifaces, hosts)
   local o = {
      widget = wibox.widget.textbox(),
      tooltip = "",
      ifaces = ifaces,
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
         get_interfaces(self.ifaces)
   }):next(
      function(results)
         local net_up = results[1]
         local status = results[2]

         local data = format_data(self.ifaces, net_up, status)

         self.widget:set_markup(data.text)
         self.tooltip = "<p>" .. data.toltip .. "</p>"
      end,
      function(err)
         print("Netmon update error: " .. err)
      end)
end

function NetMon:ssid(label)
   local iface = iface_by_label(label)
   if iface ~= nil then
      local cmd = "iwgetid --raw " .. iface
      local pipe = io.popen(cmd)
      local val = pipe:read("*a")
      pipe:close()
      return trim(val)
   end
end
-- }}}

function new(ifaces, hosts)
   local w = NetMon:new(ifaces, hosts)
   w:update()
   return w
end
