local ipairs = ipairs
local lfs, os, string, table = require("lfs"), require("os"), require("string"), require("table")
local naughty = require("naughty")

local M  = {}
_ENV = M

local base_dirs = {"/usr/share/icons", os.getenv("XDG_DATA_HOME") .. "/icons"}
local exts = {"png", "xpm", "svg", "gif"}

local themes = {}
local sizes = {}

function add_theme(theme, at_end)
   if at_end then
      table.insert(themes, theme)
   else
      table.insert(themes, 1, theme)
   end
end
function add_size(size, at_end)
   if at_end then
      table.insert(sizes, size)
   else
      table.insert(sizes, 1, size)
   end
end

function get(category, name, default)
   local dir, fn, ext, size, stat, theme
   for _, dir in ipairs(base_dirs) do
      for _, theme in ipairs(themes) do
         for _, size in ipairs(sizes) do
            for _, ext in ipairs(exts) do
               fn = string.format("%s/%s/%s/%s/%s.%s", dir, theme, size, category, name, ext)
               stat = lfs.attributes(fn)
               if stat ~= nil then
                  return fn
               end
            end
         end
      end
   end
   return default
end

function configure_naughty()
   local dirs = {}
   local dir, dirname, size, theme
   for _, dir in ipairs(base_dirs) do
      for _, theme in ipairs(themes) do
         for _, size in ipairs(sizes) do
            dirname = string.format("%s/%s/%s/apps/", dir, theme, size)
            table.insert(dirs, dirname)
         end
      end
   end
   table.insert(dirs, "/usr/share/pixmaps/")

   naughty.config.icon_dirs = dirs
   naughty.config.icon_formats = exts
end

return M