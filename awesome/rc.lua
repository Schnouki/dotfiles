-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
local hotkeys_popup = require("awful.hotkeys_popup").widget

-- Underscore (https://github.com/jtarchie/underscore-lua)
local __ = require("underscore")
-- Filesystem
local lfs = require("lfs")
-- Sockets!
local socket = require("socket")

-- Eminement dynamic tagging
require("eminent")
-- Vicious widgets
vicious = require("vicious")
vicious.contrib = require("vicious.contrib")
-- Custom widgets based on Vicious
brutal = require("brutal")
-- Markup functions
require("markup")
-- Icon theme
icon_theme = require("icon_theme")
-- Fangh calendar :)
fangh_cal = require("fangh_calendar")

-- {{{ Error handling
-- Check if awesome encountered an error during startup and fell back to
-- another config (This code will only ever execute for the fallback config)
if awesome.startup_errors then
    naughty.notify({ preset = naughty.config.presets.critical,
                     title = "Oops, there were errors during startup!",
                     text = awesome.startup_errors })
end

-- Handle runtime errors after startup
do
    local in_error = false
    awesome.connect_signal("debug::error", function (err)
        -- Make sure we don't go into an endless error loop
        if in_error then return end
        in_error = true

        naughty.notify({ preset = naughty.config.presets.critical,
                         title = "Oops, an error happened!",
                         text = tostring(err) })
        in_error = false
    end)
end
-- }}}

-- {{{ Localization
-- Requis pour la date en français
os.setlocale("fr_FR.utf8")
-- ... mais les nombres doivent être en anglais (séparateur décimal...)
os.setlocale("C", "numeric")
-- }}}

-- {{{ Variable definitions
local config_dir = awful.util.getdir("config")

-- Themes define colours, icons, font and wallpapers.
beautiful.init(config_dir .. "/theme-schnouki-zenburn.lua")
beautiful.wallpaper = function(s)
   if beautiful.wallpaper_dir then
      -- List files in wallpaper dir
      local wallpapers = {}
      local fn
      for fn in lfs.dir(beautiful.wallpaper_dir) do
         local full_fn = beautiful.wallpaper_dir .. "/" .. fn
         local mode = lfs.attributes(full_fn, "mode")
         if mode == "file" then
            table.insert(wallpapers, fn)
         end
      end

      -- Select a random wallpaper
      local rnd = math.random(#wallpapers)
      local fn = wallpapers[rnd]
      local full_fn = beautiful.wallpaper_dir .. "/" .. fn
      return full_fn
   end
end

-- This is used later as the default terminal and editor to run.
terminal = "urxvtcd"
editor = "emacsclient -n -c -a \"\""
editor_cmd = editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
local all_layouts = {
   { awful.layout.suit.tile,            true  }, -- 1
   { awful.layout.suit.tile.left,       false },
   { awful.layout.suit.tile.bottom,     true  }, -- 2
   { awful.layout.suit.tile.top,        false },
   { awful.layout.suit.fair,            true  }, -- 3
   { awful.layout.suit.fair.horizontal, true  }, -- 4
   { awful.layout.suit.spiral,          true  }, -- 5
   { awful.layout.suit.spiral.dwindle,  true  }, -- 6
   { awful.layout.suit.max,             true  }, -- 7
   { awful.layout.suit.max.fullscreen,  true  }, -- 8
   { awful.layout.suit.magnifier,       true  }, -- 9
   { awful.layout.suit.floating,        true  }, -- 10
   { awful.layout.suit.corner.nw,       true  }, -- 11
   { awful.layout.suit.corner.ne,       false },
   { awful.layout.suit.corner.sw,       false },
   { awful.layout.suit.corner.se,       false },
}
awful.layout.layouts = __.map(__.select(all_layouts, function(entry) return entry[2] end), function(entry) return entry[1] end)
local menu_layouts = __.map(all_layouts, function(entry) return entry[1] end)
-- }}}

-- {{{ Helper functions
local function client_menu_toggle_fn()
    local instance = nil

    return function ()
        if instance and instance.wibox.visible then
            instance:hide()
            instance = nil
        else
            instance = awful.menu.clients({ theme = { width = 250 } })
        end
    end
end
-- }}}

-- {{{ Menu
-- Configure the icon theme
icon_theme.add_theme("hicolor")
icon_theme.add_theme("gnome")
icon_theme.add_theme("Numix")
icon_theme.add_theme("Numix-Circle")
icon_theme.add_size("scalable")
icon_theme.add_size("48x48")
icon_theme.add_size("48")
icon_theme.add_size("32x32")
icon_theme.add_size("32")
icon_theme.add_size("16x16")
icon_theme.add_size("16")
icon_theme.configure_naughty()

-- Create a launcher widget and a main menu
myawesomemenu = {
   { "&hotkeys", function() return false, hotkeys_popup.show_help end},
   { "change &wallpaper", change_wallpapers },
   { "&manual", terminal .. " -e man awesome" },
   { "edit &config", editor_cmd .. " " .. awesome.conffile },
   { "&restart", awesome.restart },
   { "&quit", function() awesome.quit() end}
}

require("steam")
steamdir = "/home/schnouki/.local/share/Steam/SteamApps"
steammenu = { theme = {width = 200 } }
for _, game in ipairs(steam.get_games(steamdir)) do
   table.insert(steammenu, { game["name"], "steam-native steam://rungameid/" .. game["id"],
                             icon_theme.get("apps", "steam_icon_" .. game["id"]) })
end

gamesdir = "/home/schnouki/Media/Jeux"
gamemenu = {
   theme = { width = 200 },
   { "&steam", "steam-native", icon_theme.get("apps", "steam") },
   { "steam &games", steammenu, icon_theme.get("apps", "steam") },
   { "battle for wesnoth", "wesnoth", "/usr/share/icons/wesnoth-icon.png" },
   { "frozen bubble", "frozen-bubble", "/usr/share/pixmaps/frozen-bubble.png" },
   { "gplanarity", "gplanarity", "/usr/share/pixmaps/gplanarity.png" },
   { "hex-a-hop", "hex-a-hop", "/usr/share/hex-a-hop/icon.bmp" },
   { "kildclient", "kildclient", "/usr/share/pixmaps/kildclient.png" },
   { "kobo deluxe", "kobodl", "/usr/share/pixmaps/kobo-icon.xpm" },
   { "minecraft", "minecraft", icon_theme.get("apps", "minecraft") },
   { "naev", "naev", "/usr/share/pixmaps/naev.png" },
   { "open ttd", "openttd", icon_theme.get("apps", "openttd") },
   { "quake", "darkplaces-sdl -window -basedir " .. gamesdir .. "/Quake", icon_theme.get("apps", "quake") },
   { "torchlight", "torchlight", "/usr/share/pixmaps/torchlight.png" },
}

utilsmenu = {
   { "&galculator", "galculator", icon_theme.get("apps", "galculator") },
   { "qalculate-gtk", "qalculate", "/usr/share/pixmaps/qalculate.png" },
   { "&disk usage", "mate-disk-usage-analyzer", icon_theme.get("apps", "mate-disk-usage-analyzer") },
   { "g&ucharmap", "gucharmap", icon_theme.get("apps", "accessories-character-map") },
   { "&pavucontrol", "pavucontrol", icon_theme.get("apps", "multimedia-volume-control") },
}

-- {{{ Screen menu -- from Dodo
local ext_screen = "HDMI-1"
local function update_connected_screen()
   local found, m
   found = false
   awful.spawn.with_line_callback("xrandr", { stdout = function(line)
       if found then return end
       m = string.match(line, "^(%w+) connected")
       if m ~= nil and m ~= "LVDS-1" then
          ext_screen = m
          found = true
       end
   end })
end
update_connected_screen()
-- TODO: call update_connected_screen() when the list of connected screens changes

-- TODO: make this async
local function ext_screen_connected()
   local f = io.popen("xrandr")
   local line, m
   for line in f:lines() do
      m = string.match(line, "^" .. ext_screen .. " connected")
      if m == nil then
         f:close()
         return true
      end
   end
   f:close()
   return false
end

local function auto_set_screen(direction)
   local cmd = "xrandr --output LVDS-1 --auto --output " .. ext_screen
   if ext_screen_connected() then
      cmd = cmd .. " --auto --" .. direction .. " LVDS-1"
   else
      cmd = cmd .. " --off"
   end
   return os.execute(cmd)
end

local function menu_screen_text()
    return ({
          ["LVDS-1"] = "L&aptop",
          ["VGA-1"]  = "&VGA",
          ["HDMI-1"] = "&HDMI",
    })[ext_screen]
end

screenmenu = {
    { "&auto",     function() auto_set_screen("left-of") end },
    { "&clone",    "xrandr --output LVDS-1 --auto --output " .. ext_screen .. " --auto --same-as LVDS-1" },
    { "&left of",  "xrandr --output LVDS-1 --auto --output " .. ext_screen .. " --auto --left-of LVDS-1" },
    { "&right of", "xrandr --output LVDS-1 --auto --output " .. ext_screen .. " --auto --right-of LVDS-1" },
    { menu_screen_text(), function (m, menu)
        local prev = ext_screen
        ext_screen = (ext_screen == "HDMI-1" and "VGA-1" or "HDMI-1")
        m.label:set_text(menu_screen_text())
        for _, item in ipairs(menu.items) do
            if type(item.cmd) == 'string' then
                item.cmd = string.gsub(item.cmd, prev, ext_screen)
            end
        end
        return true
    end },
}
if screen.count() > 1 then
   -- TODO: make this dynamic somehow…
    table.insert(screenmenu, 2, { "&off", "xrandr --output LVDS-1 --auto --output " .. ext_screen .. " --off" })
end
-- }}}

-- {{{ Screenshot menu
local function screenshot(mode)
   local run_maim = true
   local cmd = "maim"
   local filename = os.date("%F-%H_%M_%S") .. ".png"
   local fullname = os.getenv("HOME") .. "/Dropbox/Public/Screenshots/" .. filename
   if mode == "active_window" then
      local c = client.focus
      if c ~= nil and c.content ~= nil then
         local surface = gears.surface.load(c.content, true)
         surface:write_to_png(fullname)
         run_maim = false
      end
   elseif mode == "selection" then
      cmd = cmd .. " -s -c 1,0,0,0.6"
   end
   if run_maim then
      naughty.notify({text = cmd })
      cmd = cmd .. " " .. fullname
      os.execute(cmd)
   end
   os.execute("mimeo " .. fullname)
end
screenshotmenu = {
   { "&Open dir", "geeqie " .. os.getenv("HOME") .. "/Dropbox/Public/Screenshots" },
   { "&Full screen", screenshot },
   { "&Active window", function() screenshot("active_window") end },
   { "&Selection", function() screenshot("selection") end },
}
-- }}}

-- {{{ Layouts menu
layoutsmenu = {}
for _, layout in ipairs(menu_layouts) do
   local name = awful.layout.getname(layout)
   local entry = { name, function() awful.layout.set(layout) end, beautiful["layout_" .. name] }
   table.insert(layoutsmenu, entry)
end
mylayoutsmenu = awful.menu({ items = layoutsmenu })
-- }}}

function safe_cmd(cmd)
   return function()
      return awful.spawn(cmd, true, nil, { MemoryLimit = "3G"
                                          --CPUQuota = "400%"
      })
   end
end

winemenu = {
   { "&balsamiq", safe_cmd("wine C:\\Program Files (x86)\\Balsamiq Mockups 3\\Balsamiq Mockups 3.exe"),
     "/home/schnouki/.wine/drive_c/Program Files (x86)/Balsamiq Mockups 3/icons/mockups_ico_16.png" },
}

mymainmenu = awful.menu({ items = { { "&awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "&jeux", gamemenu },
                                    { "&utils", utilsmenu },
                                    { "&layouts", layoutsmenu },
                                    { "écran &ext.", screenmenu },
                                    { "&screenshot", screenshotmenu },
                                    { "&wine", winemenu, icon_theme.get("apps", "wine") },
                                    { "&firefox", safe_cmd("firefox"), icon_theme.get("apps", "firefox") },
                                    { "&chromium", safe_cmd("chromium"), icon_theme.get("apps", "chromium") },
                                    { "fran&z", safe_cmd("franz-bin"), icon_theme.get("apps", "franz", "/usr/share/pixmaps/franz.png") },
                                    { "&gajim", "gajim", icon_theme.get("apps", "gajim") },
                                    { "t&hunderbird", safe_cmd("thunderbird"), icon_theme.get("apps", "thunderbird") },
                                    { "sp&otify", safe_cmd("spotify"), icon_theme.get("apps", "spotify-client") },
                                    { "&netflix", "chromium https://www.netflix.com/", icon_theme.get("apps", "netflix") },
                                    { "&popcorn time", safe_cmd("popcorntime"), icon_theme.get("apps", "popcorntime", "/usr/share/pixmaps/popcorntime.png") },
                                    { "li&bre office", safe_cmd("soffice"), icon_theme.get("apps", "libreoffice-writer") },
                                    { "an&droid studio", safe_cmd("android-studio"), icon_theme.get("apps", "android-studio", "/usr/share/pixmaps/android-studio.png") },
                                    { "pc&manfm", safe_cmd("pcmanfm"), icon_theme.get("apps", "system-file-manager") },
                                    { "open &terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = beautiful.awesome_icon,
                                     menu = mymainmenu })

-- Menubar configuration
menubar.utils.terminal = terminal -- Set the terminal for applications that require it
-- }}}

-- Keyboard map indicator and switcher
mykeyboardlayout = awful.widget.keyboardlayout()

-- {{{ Wibar
-- {{{   Default stuff
-- Create a textclock widget
mytextclock_icon = wibox.widget.imagebox()
mytextclock_icon:set_image(config_dir .. "/icons/time.png")
mytextclock = wibox.widget.textclock("%X", 1)
mytextclock_t = awful.tooltip({ objects = { mytextclock },
                                timer_function = function()
                                   local txt = os.date("%A %e %B %Y\n%T")
                                   txt = txt .. "\n\n" .. fangh_calendar.format_today()
                                   return txt
                                end })

-- Create a wibox for each screen and add it
local taglist_buttons = awful.util.table.join(
                    awful.button({ }, 1, function(t) t:view_only() end),
                    awful.button({ modkey }, 1, function(t)
                                              if client.focus then
                                                  client.focus:move_to_tag(t)
                                              end
                                          end),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, function(t)
                                              if client.focus then
                                                  client.focus:toggle_tag(t)
                                              end
                                          end),
                    awful.button({ }, 5, function(t) awful.tag.viewnext(t.screen) end),
                    awful.button({ }, 4, function(t) awful.tag.viewprev(t.screen) end)
                )

local tasklist_buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() and c.first_tag then
                                                      c.first_tag:view_only()
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 2, function (c) c:kill() end),
                     awful.button({ }, 3, client_menu_toggle_fn()),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                          end))
--   }}}
-- {{{   Wallpaper
local function set_wallpaper(s)
    -- Wallpaper
    if beautiful.wallpaper then
        local wallpaper = beautiful.wallpaper
        -- If wallpaper is a function, call it with the screen
        if type(wallpaper) == "function" then
            wallpaper = wallpaper(s)
        end
        gears.wallpaper.fit(wallpaper, s, "#000000")
    end
end

local function change_wallpapers()
   local s
   for s in awful.screen do
      set_wallpaper(s)
   end
end

-- Re-set wallpaper when a screen's geometry changes (e.g. different resolution)
screen.connect_signal("property::geometry", set_wallpaper)
--   }}}
-- {{{   Personal stuff
-- {{{     Helpers
-- TODO: create a helper in lousy
function gethost()
   local f = io.popen("/bin/hostname")
   local n = f:read("*a") or "none"
   f:close()
   return string.gsub(n, "\n$", "")
end

-- Personal helper library for things written in C
package.cpath = config_dir .. "/?.so;" .. package.cpath
require("lousy")

-- Backlight helper
local function round(n)
   local i, d = math.modf(n)
   if d >= 0.5 then return i+1 else return i end
end
local function get_backlight()
   local p = io.popen("light")
   local n = round(tonumber(p:read("*a")))
   p:close()
   return n
end
local function set_backlight(lvl)
   os.execute("light -S " .. lvl)
end

local backlight_notif_id = nil
local backlight_time = 0
function change_backlight(offset)
   local min_bl = 1
   local min_dt = 0.2

   local new_time = socket.gettime()
   local dt = new_time - backlight_time
   if dt < min_dt then return end
   backlight_time = new_time

   local new_bl = get_backlight() + offset
   if new_bl < min_bl then new_bl = min_bl
   elseif new_bl > 100 then new_bl = 100 end
   set_backlight(new_bl)
   local notif = naughty.notify({
         text = "Backlight level: " .. get_backlight() .. "%",
         replaces_id = backlight_notif_id,
         icon = icon_theme.get("notifications", "notification-display-brightness")
   })
   backlight_notif_id = notif.id
end

-- Screen helper
function get_next_screen()
   local s = awful.screen.focused()
   local idx = s.index + 1
   if idx > screen.count() then s = 1 end
   return awful.screen[idx]
end

-- Switch tag on next screen
function next_screen_viewnext()
   awful.tag.viewnext(get_next_screen())
end
function next_screen_viewprev()
   awful.tag.viewprev(get_next_screen())
end
-- }}}
-- {{{     Window management
-- Fenêtres "transient"
function get_transient(c)
   for k, v in pairs(client.get()) do
      if v.transient_for == c then
         return v
      end
   end
   return c
end
function new_transient(c)
   if client.focus == c.transient_for then
      client.focus = c
   end
end

-- Resize client by ratio
function resize_client_ratio(ratio, c)
   c = c or client.focus
   local geom = c:geometry()
   local coords = mouse.coords()

   -- New width and height (plus deltas)
   local nw = geom.width * ratio
   local nh = geom.height * ratio
   local dw = nw - geom.width
   local dh = nh - geom.height

   -- Mouse coordinates inside of the window
   local mx = coords.x - geom.x
   local my = coords.y - geom.y

   -- Make sure they are inside the window. If they are not, put them in the center of the window.
   if mx < 0 or mx >= geom.width then mx = geom.width / 2 end
   if my < 0 or my >= geom.height then my = geom.height / 2 end

   -- Delta for window X and Y: keep the mx/width and my/height ratios
   local dx = mx * (1 - ratio)
   local dy = my * (1 - ratio)

   -- Move and resize!
   awful.client.moveresize(dx, dy, dw, dh, c)
end

-- Titlebar
function has_titlebar(c)
   return awful.titlebar(c).widget ~= nil
end
function titlebar_visible(c)
   local _, size = c:titlebar_top()
   return size > 0
end
function show_titlebar(c)
   if has_titlebar(c) then
      awful.titlebar.show(c)
   else
      c:emit_signal("request::titlebars")
   end
end
function hide_titlebar(c)
   awful.titlebar.hide(c)
end
function toggle_titlebar(c)
   if titlebar_visible(c) then
      hide_titlebar(c)
   else
      show_titlebar(c)
   end
end
-- }}}
-- {{{     Widgets perso
separator = wibox.widget.imagebox()
separator:set_image(config_dir .. "/icons/separator.png")

require("locksmon")
locks_mon = locksmon.new()

-- {{{       Network stuff
require("netmon")
ifaces = {}
if gethost() == "baldr" then
   ifaces["E"] = "enp12s0"
   ifaces["W"] = "wlp3s0"
end
net_mon = netmon.new(ifaces, "8.8.4.4 8.8.8.8 kernel.org google.com online.net yahoo.com wikipedia.org")

-- Time-based indications using theme colors
if ifaces["W"] ~= nil then
   local function clockcheck()
      local time = tonumber(os.date("%H%M"))
      local ssid = net_mon:ssid("W")
      if ssid == nil then return end
      local at_work = ssid == "Le Paddock"
      local theme = beautiful.get()
      local new_normal = theme.bg_normal_ok
      local new_focus = theme.bg_focus_ok
      local new_urgent = theme.bg_urgent_ok
      if at_work and time >= 1730 then
         new_normal = theme.bg_normal_emerg
         new_focus = theme.bg_focus_emerg
         new_urgent = theme.bg_urgent_emerg
      elseif at_work and time >= 1700 then
         new_normal = theme.bg_normal_warn
         new_focus = theme.bg_focus_warn
         new_urgent = theme.bg_urgent_warn
      end
      if new_normal ~= theme.bg_normal or new_focus ~= theme.bg_focus or new_urgent ~= theme.bg_urgent then
         theme.bg_normal = new_normal
         theme.bg_focus = new_focus
         theme.bg_urgent = new_urgent
         if wibox_created then
            create_wibox()
         end
      end
   end
   local clockwarn = gears.timer.start_new(60, clockcheck)
   clockcheck()
end
-- }}}
-- {{{       Client info
-- Afficher des infos sur le client qui a le focus
-- d'après http://github.com/MajicOne/awesome-configs/blob/master/rc.lua
function win_info ()
   local c = client.focus

   -- Quick little short-circuit.
   if c == nil then return end

   local title, class, instance, role, type = nil, nil, nil, nil, nil
   title    = c.name
   class    = c.class
   instance = c.instance
   role     = c.role
   type     = c.type

   -- We don't want to error on nil.
   if title    == nil then title    = markup.fg.focus('nil') end
   if class    == nil then class    = markup.fg.focus('nil') end
   if instance == nil then instance = markup.fg.focus('nil') end
   if role     == nil then role     = markup.fg.focus('nil') end
   if type     == nil then type     = markup.fg.focus('nil') end

   naughty.notify({
      text = markup.fg.focus('      Role: ') .. role  .. '\n' ..
             markup.fg.focus('      Type: ') .. type  .. '\n' ..
             markup.fg.focus('      Title: ') .. title .. '\n' ..
             markup.fg.focus('    Class: ') .. class .. '\n' ..
             markup.fg.focus('Instance: ') .. instance,
      timeout = 5,
      hover_timeout = 0.5
   })
end
-- }}}
-- {{{       Mail info
-- Widget with number of unread mails if notmuch is available
local f = io.open("/usr/bin/notmuch")
if f then
   tb_mails = wibox.widget.textbox()
   tb_mails_color_normal   = "#7cb8bb" -- blue-1
   tb_mails_color_updating = "#ac7373" -- red-2
   tb_mails_color = tb_mails_color_normal
   function tb_mails_set_count(n)
      local gt0 = n > 0
      if n >= 10000 then
         n = string.format("%.1fk", n / 1000)
      end
      local s = markup.fg.color(tb_mails_color, "✉ " .. n)
      if gt0 then
         s = markup.bold(s)
      end
      tb_mails:set_markup(s)
   end
   function tb_mails_update()
      local p = io.popen("notmuch count tag:unread")
      local n = tonumber(p:read("*a"))
      io.close(p)
      if n then
         tb_mails_set_count(n)
      end
   end
   function tb_mails_updating(u)
      if u then tb_mails_color = tb_mails_color_updating
      else      tb_mails_color = tb_mails_color_normal
      end
   end
   tb_mails_update()
else
   -- Stubs for stuff needed elsewhere
   tb_mails = nil
   function tb_mails_update() end
   function tb_mails_set_count(n) end
   function tb_mails_updating(u) end
end

-- Widget with the number of mails queued in msmtpq
msmtpq_dir = "/home/schnouki/.msmtpq"
tb_msmtpq = wibox.widget.textbox()
tb_msmtpq_color = "#dfaf8f" -- orange
function tb_msmtpq_update()
   local fn, full_fn, mode
   local count = 0
   local s = ""
   for fn in lfs.dir(msmtpq_dir) do
      if string.sub(fn, -5) == ".json" then
         full_fn = msmtpq_dir .. "/" .. fn
         mode = lfs.attributes(full_fn, "mode")
         if mode == "file" then
            count = count + 1
         end
      end
   end

   if count > 0 then
      s = " [" .. count .. "]"
      s = markup.fg.color(tb_msmtpq_color, s)
      s = markup.bold(s)
   end
   tb_msmtpq:set_markup(s)
end
tb_msmtpq_update()
-- }}}
-- {{{       Media player control
function mpris2(command)
   return function()
      os.execute("~/bin/mpris2-control " .. command .. " &")
   end
end
-- }}}
-- {{{       Vicious widgets
vicious.cache(vicious.widgets.cpu)
cpu_icon = wibox.widget.imagebox()
cpu_icon:set_image(config_dir .. "/icons/cpu.png")
cpu_graph = wibox.widget {
   widget           = wibox.widget.graph,
   forced_width     = 32,
   forced_height    = 18,
   background_color = "#000000",
   color = {type = "linear", from = {0, 0}, to = {0, 18},
            stops = {{0, "#CC6666"}, {0.5, "#CCCC66"}, {1.0, "#66CC66"}}},
}
vicious.register(cpu_graph, vicious.widgets.cpu, "$1", 3)

vicious.cache(vicious.contrib.sensors)
cputemp_widget = wibox.widget {
   layout        = wibox.container.rotate,
   direction     = "east",
   forced_width  = 4,
   forced_height = 18,
   wibox.widget {
      widget           = wibox.widget.progressbar,
      background_color = "#000000",
      color = {type = "linear", from = {18, 0}, to = {0, 0},
               stops = {{0, "#CC6666"}, {0.2, "#CC66CC"}, {1.0, "#66CCCC"}}},
   },
}
vicious.register(cputemp_widget.widget, vicious.contrib.sensors,
                 function(widget, args)
                    cpu_temp = args[1]
                    return args[2]
                 end, 10, "Physical id 0")

vicious.cache(vicious.widgets.uptime)
cpu_tooltip = awful.tooltip({
      objects = { cpu_graph, cputemp_widget },
      timer_function = function()
         local txt = ""
         for idx, val in pairs(vicious.widgets.cpu()) do
            if idx >= 2 then
               txt = txt .. string.format("Core %d: %d %%\n", idx-1, val)
            end
         end

         -- Add temperature
         txt = txt .. string.format("\nTemperature: %d °C", cpu_temp)

         -- Add load average
         local uptime = vicious.widgets.uptime()
         txt = txt .. string.format("\n\nLoad: %s %s %s", uptime[4], uptime[5], uptime[6])

         return txt
      end
})


vicious.cache(vicious.widgets.mem)
mem_icon = wibox.widget.imagebox()
mem_icon:set_image(config_dir .. "/icons/mem.png")
mem_widget = wibox.layout {
   layout        = wibox.container.rotate,
   direction     = "east",
   forced_width  = 8,
   forced_height = 18,
   wibox.widget {
      widget           = wibox.widget.progressbar,
      background_color = "#000000",
      color = {type = "linear", from = {18, 0}, to = {0, 0},
               stops = {{0, "#CC6666"}, {0.5, "#CCCC66"}, {1.0, "#66CC66"}}},
   },
}
vicious.register(mem_widget.widget, vicious.widgets.mem, "$1", 3)
swap_widget = wibox.layout {
   layout        = wibox.container.rotate,
   direction     = "east",
   forced_width  = 8,
   forced_height = 18,
   wibox.widget {
      widget           = wibox.widget.progressbar,
      background_color = "#000000",
      color = {type = "linear", from = {18, 0}, to = {0, 0},
               stops = {{0, "#CC6666"}, {0.5, "#CC66CC"}, {1.0, "#6666CC"}}},
   },
}
vicious.register(swap_widget.widget, vicious.widgets.mem, "$5", 3)

bat_widget = wibox.widget.textbox()
vicious.register(bat_widget, vicious.widgets.bat,
                 function (widget, args)
                    local ret = ""
                    local state = args[1]
                    local pct = args[2]
                    local time = args[3]
                    local colors = {
                       LOW  = "#ac7373", -- red-2
                       low  = "#dfaf8f", -- orange
                       med  = "#f0dfaf", -- yellow
                       high = "#afd8af", -- green+3
                       ok   = "lightblue"
                    }
                    local col
                    if state == "⌁" or state == "↯" then
                       -- Unknown or full
                       ret = markup.fg.color(colors["ok"], pct .. "% " .. state)
                    elseif state == "+" then
                       -- Charging
                       ret = markup.fg.color(colors["high"], pct .. "% ↗")
                       if time ~= "N/A" then
                          if pct >= 75 then col = "high"
                          elseif pct < 10 then col = "med"
                          else col = "ok" end
                          ret = ret .. markup.fg.color(colors[col], " (" .. time .. ")")
                       end
                    else
                       -- Discharging
                       if pct <= 25 then col = "LOW" else col = "low" end
                       ret = markup.fg.color(colors[col], pct .. "% ↘")
                       if time ~= "N/A" then
                          if pct <= 25 then col = "LOW"
                          elseif pct <= 50 then col = "low"
                          else col = "ok" end
                          ret = ret .. markup.fg.color(colors[col], " (" .. time .. ")")
                       end
                    end
                    return " " .. ret .. " "
                 end,
                 5, "BAT0")


vol_widget = wibox.layout {
   layout        = wibox.container.rotate,
   direction     = "east",
   forced_width  = 10,
   forced_height = 18,
   wibox.widget {
      widget           = wibox.widget.progressbar,
      background_color = "#000000",
      border_color     = "#000000",
      border_width     = 1
   },
}
vicious.register(vol_widget.widget, brutal.pulse,
                 function (widget, args)
                    local col = "#6666cc"
                    local vol = args[1]
                    if args[2] == "off" then
                       col = "#666666"
                       vol = 100
                    end
                    widget:set_color(col)
                    return vol
                 end, 5)

local function get_volume(include_global_in_text)
   local txt = ""

   local data = brutal.pulse()
   local global_vol, status = data[1], data[2]
   if include_global_in_text then
      txt = string.format("<b>Volume:</b> %.1f%%", global_vol)
   end

   local sink_idx, sink_data
   local first = true
   for sink_idx, sink_data in pairs(brutal.pulse.get_role("music")) do
      local name = sink_data.prop["media.software"]
         or sink_data.prop["application.process.name"]
         or sink_data.prop["application.process.binary"]
      local volume = sink_data.volume
      if first then
         first = false
         if txt ~= "" then
            txt = txt .. "\n"
         end
      end
      if txt ~= "" then
         txt = txt .. "\n"
      end
      txt = txt .. string.format("<b>%s:</b> %.1f%%", name, volume)
   end
   return global_vol, status, txt
end

local vol_notif_id = nil
local function notify_volume()
   local global_vol, status, txt = get_volume(false)
   local icon_name = "muted"
   if status == "on" then
      if global_vol == 0 then
         icon_name = "off"
      elseif global_vol <= 33 then
         icon_name = "low"
      elseif global_vol <= 66 then
         icon_name = "medium"
      else
         icon_name = "high"
      end
   end
   local notif = naughty.notify({
         title = string.format("Volume: %.1f", global_vol),
         text = txt,
         replaces_id = vol_notif_id,
         icon = icon_theme.get("notifications", "notification-audio-volume-" .. icon_name),
         timeout = 2,
         bg = "#0000007f",
         border_width = 0
   })
   vol_notif_id = notif.id
end

vol_widget_t = awful.tooltip({
      objects = { vol_widget },
      timer_function = function()
         local global_vol, status, txt = get_volume()
         return txt
      end
})


function volume_up()   brutal.pulse.add( 5)  vicious.force({vol_widget}) notify_volume() end
function volume_down() brutal.pulse.add(-5)  vicious.force({vol_widget}) notify_volume() end
function volume_mute() brutal.pulse.toggle() vicious.force({vol_widget}) end
function volume_update() vicious.force({ vol_widget }) end
function volume_music_up()   brutal.pulse.add_role( 5, "music") notify_volume() end
function volume_music_down() brutal.pulse.add_role(-5, "music") notify_volume() end

vol_widget:buttons(awful.util.table.join(
       awful.button({ }, 1, function () awful.spawn("pavucontrol") end),
       awful.button({ }, 2, volume_mute),
       awful.button({ }, 3, function () brutal.pulse.menu(volume_update):toggle() end),
       awful.button({ }, 4, volume_up),
       awful.button({ }, 5, volume_down),
       awful.button({ "Shift" }, 4, volume_music_up),
       awful.button({ "Shift" }, 5, volume_music_down)
))
-- }}}
-- }}}
-- {{{     Raccourcis claviers persos
persokeys = {
   -- Volume
   awful.key({ }, "XF86AudioRaiseVolume", volume_up),
   awful.key({ }, "XF86AudioLowerVolume", volume_down),
   awful.key({ }, "XF86AudioMute",        volume_mute),

   awful.key({ modkey }, "Up",        volume_up,   {description="raise volume", group="audio"}),
   awful.key({ modkey }, "Down",      volume_down, {description="lower volume", group="audio"}),
   awful.key({ modkey }, "KP_Delete", volume_mute, {description="toggle mute",  group="audio"}),

   awful.key({ "Shift" }, "XF86AudioRaiseVolume", volume_music_up),
   awful.key({ "Shift" }, "XF86AudioLowerVolume", volume_music_down),
   awful.key({ modkey, "Shift" }, "Up",           volume_music_up,   {description="raise music volume", group="audio"}),
   awful.key({ modkey, "Shift" }, "Down",         volume_music_down, {description="lower music volume", group="audio"}),

   -- Luminosité
   awful.key({ }, "XF86MonBrightnessUp",   function () change_backlight( 5) end),
   awful.key({ }, "XF86MonBrightnessDown", function () change_backlight(-5) end),

   -- F1 - mettre en veille
   awful.key({ modkey, "Control" }, "F1", function () awful.spawn("gksudo systemctl suspend") end,
             { description="suspend", group="power management" }),
   -- F2 - verrouiller l'écran
   awful.key({ }, "XF86ScreenSaver",      function () awful.spawn("xscreensaver-command -lock") end),
   awful.key({ modkey, "Control" }, "F2", function () awful.spawn("xscreensaver-command -lock") end,
             { description="lock screen", group="power management" }),
   -- F3 - éteindre l'écran
   awful.key({ }, "XF86Battery",          function () awful.spawn("xset dpms force suspend") end),
   awful.key({ modkey, "Control" }, "F3", function () awful.spawn("xset dpms force suspend") end,
             { description="turn screen off", group="power management" }),

   -- Caps Lock et Num Lock
   awful.key({ }, "Num_Lock",  function() locks_mon:update("Num_Lock")  end),
   awful.key({ }, "Caps_Lock", function() locks_mon:update("Caps_Lock") end),

   -- Touches tag suivant/précédent au-dessus du pavé numérique
   awful.key({ }, "XF86Back",     awful.tag.viewprev),
   awful.key({ }, "XF86Forward" , awful.tag.viewnext),

   -- Tag suivant/précédent sur l'autre écran avec PageUp/PageDown
   awful.key({ modkey }, "Prior", next_screen_viewprev,
             { description="view previous on next screen", group="tag" }),
   awful.key({ modkey }, "Next",  next_screen_viewnext,
             { description="view next on next screen", group="tag" }),

   -- Éditeur de texte avec la touche ThinkVantage
   awful.key({                 }, "XF86Launch1", function () awful.spawn(editor_cmd) end),
   awful.key({ modkey          }, "KP_Insert",   function () awful.spawn(editor_cmd) end),
   awful.key({ modkey, "Shift" }, "Return",      function () awful.spawn(editor_cmd) end),

   -- Media player
   awful.key({ }, "XF86AudioStop", mpris2("stop")),
   awful.key({ }, "XF86AudioPlay", mpris2("playpause")),
   awful.key({ }, "XF86AudioPrev", mpris2("prev")),
   awful.key({ }, "XF86AudioNext", mpris2("next")),

   awful.key({ modkey, "Control" }, "Up",    mpris2("stop"),
             { description="stop", group="media" }),
   awful.key({ modkey, "Control" }, "Down",  mpris2("playpause"),
             { description="play/pause", group="media" }),
   awful.key({ modkey, "Control" }, "Left",  mpris2("prev"),
             { description="previous", group="media" }),
   awful.key({ modkey, "Control" }, "Right", mpris2("next"),
             { description="next", group="media" }),

   awful.key({ modkey, "Shift"   }, "d", function () awful.spawn("dspop") end,
             { description="run dspop", group="media" }),
   awful.key({ modkey, "Control" }, "d", function () awful.spawn("dmpd") end,
             { description="run dmpd", group="media" }),
   awful.key({ modkey, "Shift"   }, "i", mpris2("info"),
             { description="show MPRIS2 info", group="media" }),
   awful.key({ modkey, "Shift"   }, "o", mpris2("nextplayer"),
             { description="switch to the next MPRIS2 player", group="media" }),

   -- Misc
   awful.key({ modkey          }, "Print", screenshot,
             { description="take a screenshot with Shutter", group="misc" }),
   awful.key({ modkey, "Shift" }, "f",     function() awful.mouse.finder():find() end,
             { description="locate pointer", group="misc" }),
   awful.key({ modkey, "Shift" }, "w",     function () change_wallpapers() end,
             { description="change wallpaper", group="misc" }),
}

persoclientkeys = {
   awful.key({ modkey, "Shift"   }, "t",           toggle_titlebar,
             { description="toggle titlebar", group="client" }),
   awful.key({ modkey            }, "s",           function (c) c.sticky = not c.sticky end,
             { description="toggle sticky", group="client" }),
   awful.key({ modkey, "Control" }, "i",           win_info,
             { description="show info", group="client" }),
   awful.key({ modkey            }, "KP_Add",      function (c) resize_client_ratio(1.1, c) end,
             { description="increase size by 10%", group="client" }),
   awful.key({ modkey            }, "KP_Subtract", function (c) resize_client_ratio(0.9, c) end,
             { description="decrease size by 10%", group="client" }),
   awful.key({ modkey, "Control" }, "KP_Add",      function (c) resize_client_ratio(1.5, c) end,
             { description="increase size by 50%", group="client" }),
   awful.key({ modkey, "Control" }, "KP_Subtract", function (c) resize_client_ratio(0.5, c) end,
             { description="decrease size by 50%", group="client" }),
}
-- }}}
-- }}}
-- {{{   Setup screens
awful.screen.connect_for_each_screen(function(s)
    -- Wallpaper
    set_wallpaper(s)

    -- Each screen has its own tag table.
    local tags = { "1", "2", "3", "4", "5", "6", "7", "8", "9" }
    if s.index == 1 then tags = {"‽", "📧", "✪", "💼", "💻", "6", "7", "8", "⌘"} end
    awful.tag(tags, s, awful.layout.layouts[1])

    -- Set the last tag to floating
    -- TODO

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc( 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(-1) end),
                           awful.button({ }, 4, function () awful.layout.inc( 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(-1) end)))
    -- Create a taglist widget
    s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all, taglist_buttons)

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, tasklist_buttons)

    -- Create the wibox
    s.mywibox = awful.wibar({ position = "top", screen = s })

    -- Add widgets to the wibox
    local right_widgets = __.concat(
       { separator, tb_mails, tb_msmtpq, separator,
         cpu_icon, cpu_graph, cputemp_widget, mem_icon, mem_widget, swap_widget, separator,
         net_mon.widget,
       },
       { ip_mon and ip_mon.widget or nil },
       { bat_widget, vol_widget, separator,
         locks_mon.widget,
       },
       { mykeyboardlayout },
       { s.index == 1 and wibox.widget.systray() or nil },
       {
            mytextclock_icon,
            mytextclock,
            s.mylayoutbox,
       }
    )
    right_widgets.layout = wibox.layout.fixed.horizontal

    s.mywibox:setup {
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            mylauncher,
            s.mytaglist,
            s.mypromptbox,
        },
        s.mytasklist, -- Middle widget
        right_widgets,
    }
end)
--   }}}
-- }}}

-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 5, awful.tag.viewnext),
    awful.button({ }, 4, awful.tag.viewprev)
))
-- }}}

-- {{{ Tag management (from the "tag" doc)
local function delete_tag()
    local t = awful.screen.focused().selected_tag
    if not t then return end
    t:delete()
end

local function add_tag()
    awful.tag.add("NewTag",{screen= awful.screen.focused() }):view_only()
end

local function rename_tag()
    awful.prompt.run {
        prompt       = "New tag name: ",
        textbox      = awful.screen.focused().mypromptbox.widget,
        exe_callback = function(new_name)
            if not new_name or #new_name == 0 then return end

            local t = awful.screen.focused().selected_tag
            if t then
                t.name = new_name
            end
        end
    }
end

local function move_to_new_tag()
    local c = client.focus
    if not c then return end

    local t = awful.tag.add(c.class,{screen= c.screen })
    c:tags({t})
    t:view_only()
end

local function move_tag(delta)
   local t = awful.screen.focused().selected_tag
   if not t then return end
   t.index = t.index + delta
end
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    awful.key({ modkey,           }, "s",      hotkeys_popup.show_help,
              {description="show help", group="awesome"}),
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev,
              {description = "view previous", group = "tag"}),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext,
              {description = "view next", group = "tag"}),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore,
              {description = "go back", group = "tag"}),

    awful.key({ modkey,           }, "a", add_tag,
              {description = "add a tag", group = "tag"}),
    awful.key({ modkey, "Shift"   }, "a", delete_tag,
              {description = "delete the current tag", group = "tag"}),
    awful.key({ modkey, "Control"   }, "a", move_to_new_tag,
              {description = "add a tag with the focused client", group = "tag"}),
    awful.key({ modkey, "Shift"   }, "r", rename_tag,
              {description = "rename the current tag", group = "tag"}),
    awful.key({ modkey, "Shift"   }, "Next", function () move_tag(1) end,
              {description = "move right", group="tag"}),
    awful.key({ modkey, "Shift"   }, "Prior", function () move_tag(-1) end,
              {description = "move left", group="tag"}),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
        end,
        {description = "focus next by index", group = "client"}
    ),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
        end,
        {description = "focus previous by index", group = "client"}
    ),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end,
              {description = "show main menu", group = "awesome"}),
    awful.key({ modkey, "Control" }, "w", function () mylayoutmenu:show() end,
              {description = "show layouts menu", group = "awesome" }),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end,
              {description = "swap with next client by index", group = "client"}),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end,
              {description = "swap with previous client by index", group = "client"}),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end,
              {description = "focus the next screen", group = "screen"}),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end,
              {description = "focus the previous screen", group = "screen"}),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,
              {description = "jump to urgent client", group = "client"}),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        {description = "go back", group = "client"}),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.spawn(terminal) end,
              {description = "open a terminal", group = "launcher"}),
    awful.key({ modkey, "Control" }, "r", awesome.restart,
              {description = "reload awesome", group = "awesome"}),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit,
              {description = "quit awesome", group = "awesome"}),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)          end,
              {description = "increase master width factor", group = "layout"}),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)          end,
              {description = "decrease master width factor", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1, nil, true) end,
              {description = "increase the number of master clients", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1, nil, true) end,
              {description = "decrease the number of master clients", group = "layout"}),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1, nil, true)    end,
              {description = "increase the number of columns", group = "layout"}),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1, nil, true)    end,
              {description = "decrease the number of columns", group = "layout"}),
    awful.key({ modkey,           }, "space", function () awful.layout.inc( 1)                end,
              {description = "select next", group = "layout"}),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(-1)                end,
              {description = "select previous", group = "layout"}),

    awful.key({ modkey, "Control" }, "n",
              function ()
                  local c = awful.client.restore()
                  -- Focus restored client
                  if c then
                      client.focus = c
                      c:raise()
                  end
              end,
              {description = "restore minimized", group = "client"}),

    -- Prompt
    awful.key({ modkey },            "r",     function () awful.screen.focused().mypromptbox:run() end,
              {description = "run prompt", group = "launcher"}),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run {
                    prompt       = "Run Lua code: ",
                    textbox      = awful.screen.focused().mypromptbox.widget,
                    exe_callback = awful.util.eval,
                    history_path = awful.util.get_cache_dir() .. "/history_eval"
                  }
              end,
              {description = "lua execute prompt", group = "awesome"}),
    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end,
              {description = "show the menubar", group = "launcher"})
)

clientkeys = awful.util.table.join(
    awful.key({ modkey,           }, "f",
        function (c)
            c.fullscreen = not c.fullscreen
            c:raise()
        end,
        {description = "toggle fullscreen", group = "client"}),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end,
              {description = "close", group = "client"}),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ,
              {description = "toggle floating", group = "client"}),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end,
              {description = "move to master", group = "client"}),
    awful.key({ modkey,           }, "o",      function (c) c:move_to_screen()               end,
              {description = "move to screen", group = "client"}),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end,
              {description = "toggle keep on top", group = "client"}),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end ,
        {description = "minimize", group = "client"}),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized = not c.maximized
            c:raise()
        end ,
        {description = "maximize", group = "client"})
)

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, 9 do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = awful.screen.focused()
                        local tag = screen.tags[i]
                        if tag then
                           tag:view_only()
                        end
                  end,
                  {description = "view tag #"..i, group = "tag"}),
        -- Toggle tag display.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = awful.screen.focused()
                      local tag = screen.tags[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end,
                  {description = "toggle tag #" .. i, group = "tag"}),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:move_to_tag(tag)
                          end
                     end
                  end,
                  {description = "move focused client to tag #"..i, group = "tag"}),
        -- Toggle tag on focused client.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = client.focus.screen.tags[i]
                          if tag then
                              client.focus:toggle_tag(tag)
                          end
                      end
                  end,
                  {description = "toggle focused client on tag #" .. i, group = "tag"})
    )
end

clientbuttons = awful.util.table.join(
    awful.button({ }, 1, function (c) client.focus = c; c:raise() end),
    awful.button({ modkey }, 1, awful.mouse.client.move),
    awful.button({ modkey }, 3, awful.mouse.client.resize))

-- Set keys
for k, v in pairs(persokeys) do
   for _, vv in pairs(v) do
      table.insert(globalkeys, vv)
   end
end
for k, v in pairs(persoclientkeys) do
   for _, vv in pairs(v) do
      table.insert(clientkeys, vv)
   end
end
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Rules to apply to new clients (through the "manage" signal).
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = awful.client.focus.filter,
                     raise = true,
                     keys = clientkeys,
                     buttons = clientbuttons,
                     screen = awful.screen.preferred,
                     placement = awful.placement.no_overlap+awful.placement.no_offscreen
     }
    },

    -- Floating clients.
    { rule_any = {
         instance = {
            "crx_eggkanocgddhmamlbiijnphhppkpkmkl", -- Tab Outliner
            "pinentry-gtk-2",
            "popcorntime",
            "wpa_gui" ,
        },
        class = {
           "BBQScreenClient2",
           "Galculator",
           "Gmpc",
           "Gtk-recordMyDesktop",
           "mpv",
           "pinentry",
           "Plugin-container",
           "Smplayer",
           "Vlc",
           "Wine",
        },

        name = {
           "Android Device Monitor",
           "Gnuplot (window id : 0)",
           "Minecraft",
           "Popcorn Time",
        },
        role = {
          "AlarmWindow",  -- Thunderbird's calendar.
          "pop-up",       -- e.g. Google Chrome's (detached) Developer Tools.
        }
      }, properties = { floating = true }},

    -- Add titlebars to dialogs and utility clients
    { rule_any = {type = { "dialog", "utility" }
      }, properties = { titlebars_enabled = true }
    },

    -- Set Firefox to always map on the tag named "2" on screen 1.
    -- { rule = { class = "Firefox" },
    --   properties = { screen = 1, tag = "2" } },

    -- Firefox: only the main window isn't floating
    { rule = { class = "Firefox" }, except = { instance = "Navigator" },
      properties = { floating = true } },

    -- Keep some applications on the last tag of the first screen
    { rule_any = { instance = { "spotify.exe", "spotify", "Steam" } },
      properties = { floating = true, tag="⌘" } },

    -- Other applications must be floating *and* centered
    { rule_any = { class = {"Arandr", "Pavucontrol" } },
      properties = { floating = true,
                     placement = awful.placement.centered } },
    { rule = { class = "Skype", role = "CallWindow" },
      properties = { floating = true,
                     placement = awful.placement.centered } },

    -- VirtualBox: TODO
    { rule = { class = "VirtualBox" },
      properties = { maximized = true } },
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c)
    -- Set the windows at the slave,
    -- i.e. put it at the end of others instead of setting it master.
    -- if not awesome.startup then awful.client.setslave(c) end

    if awesome.startup and
      not c.size_hints.user_position
      and not c.size_hints.program_position then
        -- Prevent clients from being unreachable after screen count changes.
        awful.placement.no_offscreen(c)
    end
end)

-- Add a titlebar if titlebars_enabled is set to true in the rules.
client.connect_signal("request::titlebars", function(c)
    -- buttons for the titlebar
    local buttons = awful.util.table.join(
        awful.button({ }, 1, function()
            client.focus = c
            c:raise()
            awful.mouse.client.move(c)
        end),
        awful.button({ }, 3, function()
            client.focus = c
            c:raise()
            awful.mouse.client.resize(c)
        end)
    )

    awful.titlebar(c) : setup {
        { -- Left
            awful.titlebar.widget.iconwidget(c),
            buttons = buttons,
            layout  = wibox.layout.fixed.horizontal
        },
        { -- Middle
            { -- Title
                align  = "center",
                widget = awful.titlebar.widget.titlewidget(c)
            },
            buttons = buttons,
            layout  = wibox.layout.flex.horizontal
        },
        { -- Right
            awful.titlebar.widget.floatingbutton (c),
            awful.titlebar.widget.maximizedbutton(c),
            awful.titlebar.widget.stickybutton   (c),
            awful.titlebar.widget.ontopbutton    (c),
            awful.titlebar.widget.closebutton    (c),
            layout = wibox.layout.fixed.horizontal()
        },
        layout = wibox.layout.align.horizontal
    }
end)

client.connect_signal("property::floating", function(c)
    if c.floating then
       show_titlebar(c)
    else
       hide_titlebar(c)
    end
end)

-- Enable sloppy focus, so that focus follows mouse.
client.connect_signal("mouse::enter", function(c)
    if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
        and awful.client.focus.filter(c) then
           client.focus = get_transient(c)
    end
end)
client.connect_signal("manage", function(c)
    new_transient(c)
end)

client.connect_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
-- {{{ Timers
mytimer15 = gears.timer.start_new(15, function ()
    net_mon:update()
    if ip_mon then ip_mon:update() end
    tb_mails_update()
    tb_msmtpq_update()
end)

mytimer300 = gears.timer.start_new(300, function()
    change_wallpapers()
end)
-- }}}
-- {{{ Run applications in a different (transient) systemd scope
-- This allows awesome to be restarted by systemd without killing the
-- applications started from it.
local oldspawn = awful.spawn
awful.spawn = function (cmd, sn_rules, cb, sd_params)
   if sd_params ~= nil then
      sd_params = __.concat(
         {"-p"},
         __.map(sd_params, function(v, k) return k .. "=" .. v end)
      )
   else
      sd_params = {}
   end

   if type(cmd) == "table" then
      cmd = __.concat(
         {"systemd-run", "--user", "--scope"}, sd_params,
         cmd
      )

   else
      cmd = "systemd-run --user --scope " .. __.join(sd_params, " ") .. " " .. cmd
   end

   -- naughty.notify({ text = cmd })
   return oldspawn(cmd, sn_rules, cb)
end
-- }}}
