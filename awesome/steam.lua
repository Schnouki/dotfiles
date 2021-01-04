local ipairs = ipairs
local io = require("io")
local string = require("string")
local table = require("table")

local lfs = require("lfs")

local M = {}
_ENV = M

local function get_all_acf(path)
   local filenames = {}
   for filename in lfs.dir(path) do
      if string.match(filename, ".acf$") then
         table.insert(filenames, path .. "/" .. filename)
      end
   end
   return filenames
end

local function parse_acf(filename)
   local line, k, v
   local info = {}
   for line in io.lines(filename) do
      k, v = string.match(line, "%s*\"(%w+)\"%s+\"([^\"]+)\"")
      if k ~= nil and v ~= nil then
         if k == "appid" then
            info["id"] = v
         elseif k == "name" then
            info["name"] = v
         end
      end
   end
   return info
end

function get_games(path)
   local filename, game_info
   local acf = get_all_acf(path)
   local games = {}
   for _, filename in ipairs(acf) do
      game_info = parse_acf(filename)
      table.insert(games, game_info)
   end
   table.sort(games, function(a, b) return a["name"] < b["name"] end)
   return games
end

return M
