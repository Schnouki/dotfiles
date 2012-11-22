local io, os, string, ipairs, tonumber, setmetatable = require("io"), require("os"), require("string"), ipairs, tonumber, setmetatable
local widget = require("widget")

module("nvtemp")

local colors = {
   high = "#ac7373", -- red-2
   med  = "#f0dfaf", -- yellow
   low  = "#afd8af"  -- green+3
}

-- {{{ Internals
local status_file = os.tmpname()

local function query_temp()
   local cmd = "nvidia-smi -d TEMPERATURE -a > " .. status_file .. " &"
   os.execute(cmd)
end

local function get_temps()
   local temps = {}
   local f = io.open(status_file)

   if f then
      local temp_section = false
      local sect, name, temp

      local temp
      for line in f:lines() do
         temp = string.match(line, "^%s+Gpu%s+: (%d+) C")
         if temp then
            temps[#temps + 1] = tonumber(temp)
         end
      end
   end
   return temps
end

local function format_temp(temp)
   local level
   if temp >= 80 then
      level = "high"
   elseif temp >= 65 then
      level = "med"
   else
      level = "low"
   end
   return '<span foreground="' .. colors[level] .. '">' .. temp .. '</span>'
end
-- }}}
-- {{{ Class definition
local NVTempMon = {}

function NVTempMon:new()
   local o = { widget = widget({ type = "textbox" }) }
   setmetatable(o, self)
   self.__index = self
   return o
end

function NVTempMon:update()
   local s = ""
   local temps = get_temps()
   query_temp()

   for i, temp in ipairs(temps) do
      if i > 1 then s = s .. ":" end
      s = s .. format_temp(temp)
   end

   if #s == 0 then
      s = "n/a"
   else
      s = s .. "°C"
   end
   self.widget.text = s
end
-- }}}

function new()
   local w = NVTempMon:new()
   w:update()
   return w
end
