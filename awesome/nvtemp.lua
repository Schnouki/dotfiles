local io, os, string, tonumber = require("io"), require("os"), require("string"), tonumber
local status_file

module("nvtemp")

function init()
   status_file = os.tmpname()
end

function query()
   local cmd = "~/.config/awesome/nvtemp.sh > " .. status_file .. " &"
   os.execute(cmd)
end

function format()
   local s = ""
   local f = io.open(status_file)

   if not f then
      query()
      return "GPU: ..."
   end

   for line in f:lines() do
      local temp = tonumber(line)
      local color = "green"

      if temp >= 80 then
         color = "red"
      elseif temp >= 65 then
         color = "yellow"
      end

      s = s .. '<span foreground="' .. color .. '">' .. line .. '</span>/'
   end

   f:close()
   query()
   if #s > 0 then
      return "GPU: " .. string.sub(s, 1, -2) .. "°C"
   else
      return "GPU: ..."
   end
end
