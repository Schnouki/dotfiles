local io, os, string, tonumber = require("io"), require("os"), require("string"), tonumber
local status_file

module("nvtemp")

function init()
   status_file = os.tmpname()
end

function query()
   local cmd = "nvidia-smi -a > " .. status_file .. " &"
   os.execute(cmd)
end

function format_temp_gpu(temp)
   local color = "green"
   if temp >= 80 then
      color = "red"
   elseif temp >= 65 then
      color = "yellow"
   end
   return '<span foreground="' .. color .. '">' .. temp .. '</span>:'
end

function format_temp_board(temp)
   local color = "green"
   if temp >= 65 then
      color = "red"
   elseif temp >= 50 then
      color = "yellow"
   end
   return '<span foreground="' .. color .. '">' .. temp .. '</span>/'
end

function format()
   local s = ""
   local f = io.open(status_file)

   if not f then
      query()
      return "GPU: n/a"
   end

   local temp_section = false
   local sect, name, temp
   for line in f:lines() do
      -- Temperature section?
      sect = string.match(line, "^    (%w+)")
      if sect then
         temp_section = (sect == "Temperature")
      end

      -- Temperatures?
      if temp_section then
         name, temp = string.match(line, "^%s+(%w+)%s+: (%d+)")
         if name then
            temp = tonumber(temp)
            if name == "Gpu" then
               s = s .. format_temp_gpu(temp)
            elseif name == "Board" then
               s = s .. format_temp_board(temp)
            end
         end
      end
   end

   f:close()
   query()
   if #s > 0 then
      return string.sub(s, 1, -2) .. "°C"
   else
      return "n/a"
   end
end
