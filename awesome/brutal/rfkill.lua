local io = { popen = io.popen }
local os = { execute = os.execute }
local pairs = pairs
local string = {
   char = string.char,
   sub = string.sub
}
local tonumber = tonumber

local lpeg = require("lpeg")

--------------------------------------------------------------------------------

local C, Cc, Cf, Cg, Cs, Ct, P, R, S = lpeg.C, lpeg.Cc, lpeg.Cf, lpeg.Cg, lpeg.Cs, lpeg.Ct, lpeg.P, lpeg.R, lpeg.S

local function hex2char(h)
   local ret = string.char(tonumber(string.sub(h, 3), 16))
   return ret
end

local SP = S" \n\t"
local HEX = R("09", "af", "AF")
local HEXCHAR = (P"\\x" * HEX^1) / hex2char
local CHAR = (1 - SP)
local WORD = Cs((HEXCHAR + CHAR)^1) * SP^1

local status_line = Cg(WORD * Ct(Cg(WORD, "name") * Cg(WORD, "status")))
local status_lines = Cf(Ct("") * status_line^1, rawset)

--------------------------------------------------------------------------------

local function get_status()
   local f = io.popen("LC_ALL=C rfkill -nro TYPE,TYPE-DESC,SOFT")
   if f == nil then
      return nil
   else
      local data = f:read("*all")
      f:close()
      return status_lines:match(data)
   end
end

local function toggler(device, blocked)
   local cmd = "rfkill "
   if blocked then
      cmd = cmd .. "unblock"
   else
      cmd = cmd .. "block"
   end
   cmd = cmd .. " " .. device
   return function()
      os.execute(cmd)
   end
end

function menu()
   local status = get_status()
   local menu = {}
   local device, data
   for device, data in pairs(status) do
      local prefix = "    "
      if data.status ~= "blocked" then prefix = "✓ " end
      table.insert(menu, { prefix .. data.name,
                           toggler(device, data.status == "blocked") })
   end
   return menu
end

return {
   menu = menu
}
