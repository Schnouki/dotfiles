-- Standard awesome library
local gears = require("gears")
local awful = require("awful")
awful.rules = require("awful.rules")
awful.mouse.finder = require("awful.mouse.finder")
require("awful.autofocus")
-- Widget and layout library
local wibox = require("wibox")
-- Theme handling library
local beautiful = require("beautiful")
-- Notification library
local naughty = require("naughty")
local menubar = require("menubar")
-- Underscore (https://github.com/jtarchie/underscore-lua)
local _ = require("underscore")
-- Filesystem
local lfs = require("lfs")

-- Eminent dynamic tagging
require("eminent")
-- Vicious widgets
vicious = require("vicious")
vicious.contrib = require("vicious.contrib")
-- Custom widgets based on Vicious
brutal = require("brutal")
-- Markup functions
require("markup")
-- Pomodoro widget
pomodoro = require("pomodoro")
-- Icon theme
icon_theme = require("icon_theme")
-- Fangh calendar :)
fangh_calendar = require("fangh_calendar")

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
                         text = err })
        in_error = false
    end)
end
-- }}}

-- Requis pour la date en français
os.setlocale("fr_FR.utf8")
-- ... mais les nombres doivent être en anglais (séparateur décimal...)
os.setlocale("C", "numeric")

-- {{{ Variable definitions
-- Themes define colours, icons, and wallpapers
local config_dir = awful.util.getdir("config")
beautiful.init(config_dir .. "/themes/schnouki-zenburn.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvtcd"
editor = "emacsclient -n -c -a \"\""
-- editor_cmd = terminal .. " -e " .. editor
editor_cmd = editor

-- Default modkey.
-- Usually, Mod4 is the key with a logo between Control and Alt.
-- If you do not like this or do not have such a key,
-- I suggest you to remap Mod4 to another key using xmodmap or other tools.
-- However, you can use another modifier like Mod1, but it may interact with others.
modkey = "Mod4"

-- Table of layouts to cover with awful.layout.inc, order matters.
local layouts =
{
    awful.layout.suit.tile,            --  1
    awful.layout.suit.tile.bottom,     --  2
    awful.layout.suit.fair,            --  3
    awful.layout.suit.fair.horizontal, --  4
    awful.layout.suit.max,             --  5
    awful.layout.suit.max.fullscreen,  --  6
    awful.layout.suit.magnifier,       --  7
    awful.layout.suit.floating,        --  8
    awful.layout.suit.spiral,          --  9
    awful.layout.suit.spiral.dwindle,  -- 10
}
-- }}}

-- {{{ Wallpaper
function change_wallpapers(show_notif)
   if beautiful.wallpaper_dir then
      -- List files in wallpaper dir
      local wallpapers = {}
      for fn in lfs.dir(beautiful.wallpaper_dir) do
         local full_fn = beautiful.wallpaper_dir .. "/" .. fn
         local mode = lfs.attributes(full_fn, "mode")
         if mode == "file" then
            table.insert(wallpapers, fn)
         end
      end

      -- Set a random wallpaper on each screen
      local notif_txt = ""
      for s = 1, screen.count() do
         local rnd = math.random(#wallpapers)
         local fn = wallpapers[rnd]
         local full_fn = beautiful.wallpaper_dir .. "/" .. fn
         gears.wallpaper.fit(full_fn, s, "#000000")

         if s > 1 then notif_txt = notif_txt .. "\n" end
         notif_txt = notif_txt .. "<b>" .. s .. ":</b> " .. fn
      end

      if show_notif then
         naughty.notify({ title = "New wallpapers", text = notif_txt })
      end
   end
end

-- Change the wallpaper 2s after startup (needed for dealing with multiple screens)
(function()
   local wp_timer = timer { timeout = 2 }
   wp_timer:connect_signal("timeout", function()
                           change_wallpapers()
                           wp_timer:stop()
   end)
   wp_timer:start()
   change_wallpapers()
end)()
-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
tags[1] = awful.tag({"‽", "✉", "✪", "➍", "➎", "➏", "➐", "☮", "♫"}, 1, layouts[1])
for s = 2, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({"➊", "➋", "➌", "➍", "➎", "➏", "➐", "➑", "➒" }, s, layouts[1])
end

-- Customize some tags
awful.tag.setmwfact(0.72, tags[screen.count()][1])
awful.tag.setmwfact(0.72, tags[1][2])
awful.tag.setmwfact(0.65, tags[1][3])
awful.tag.setmwfact(0.65, tags[1][8])

-- Set last tag of each screen to floating
for s = 1, screen.count() do
   awful.layout.set(layouts[8], tags[s][9])
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

-- Create a laucher widget and a main menu
myawesomemenu = {
   { "change &wallpaper", function () change_wallpapers(true) end },
   { "&manual", terminal .. " -e man awesome" },
   { "edit &config", editor_cmd .. " " .. awesome.conffile },
   { "&restart", awesome.restart },
   { "&quit", awesome.quit }
}

require("steam")
steamdir = "/home/schnouki/.local/share/Steam/SteamApps"
steammenu = { theme = {width = 200 } }
for _, game in ipairs(steam.get_games(steamdir)) do
   table.insert(steammenu, { game["name"], "steam steam://rungameid/" .. game["id"],
                             icon_theme.get("apps", "steam_icon_" .. game["id"]) })
end

gamesdir = "/home/schnouki/Media/Jeux"
gamemenu = {
   theme = { width = 200 },
   { "&steam", "steam", icon_theme.get("apps", "steam") },
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
   { "gd&map", "gdmap", "/usr/share/pixmaps/gdmap_icon.png" },
   { "g&ucharmap", "gucharmap", icon_theme.get("apps", "accessories-character-map") },
   { "&pavucontrol", "pavucontrol", icon_theme.get("apps", "multimedia-volume-control") },
}

-- {{{ Screen menu -- from Dodo
local function get_connected_screen(default)
   local f = io.popen("xrandr")
   local line, m
   for line in f:lines() do
      m = string.match(line, "^(%w+) connected")
      if m ~= nil and m ~= "LVDS-1" then
         f:close()
         return m
      end
   end
   f:close()
   return default
end
local ext_screen = get_connected_screen("HDMI-1")

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
for _, layout in ipairs(layouts) do
   local name = awful.layout.getname(layout)
   local entry = { name, function() awful.layout.set(name) end, beautiful["layout_" .. name] }
   table.insert(layoutsmenu, entry)
end
mylayoutsmenu = awful.menu({ items = layoutsmenu })
-- }}}

function safe_cmd(cmd)
   return function()
      return awful.util.spawn(cmd, true, {
                                 MemoryLimit = "3G",
                                 CPUQuota = "400%"
      })
   end
end

mymainmenu = awful.menu({ items = { { "&awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "&jeux", gamemenu },
                                    { "&utils", utilsmenu },
                                    { "&layouts", layoutsmenu },
                                    { "écran &ext.", screenmenu },
                                    { "&screenshot", screenshotmenu },
                                    { "&firefox", safe_cmd("firefox"), icon_theme.get("apps", "firefox") },
                                    { "&chromium", safe_cmd("chromium"), icon_theme.get("apps", "chromium") },
                                    { "&gajim", "gajim", icon_theme.get("apps", "gajim") },
                                    { "t&hunderbird", safe_cmd("thunderbird"), icon_theme.get("apps", "thunderbird") },
                                    { "sp&otify", "spotify", icon_theme.get("apps", "spotify-client") },
                                    { "&netflix", "chromium https://www.netflix.com/", icon_theme.get("apps", "netflix") },
                                    { "&popcorn time", safe_cmd("popcorntime"), icon_theme.get("apps", "popcorntime", "/usr/share/pixmaps/popcorntime.png") },
                                    { "li&bre office", "soffice", icon_theme.get("apps", "libreoffice-writer") },
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

-- {{{ Wibox
-- {{{   Default stuff
-- Create a textclock widget
mytextclock_icon = wibox.widget.imagebox()
mytextclock_icon:set_image(config_dir .. "/icons/time.png")
mytextclock = awful.widget.textclock("%X", 1)
mytextclock_t = awful.tooltip({ objects = { mytextclock },
                                timer_function = function()
                                   local txt = os.date("%A %e %B %Y\n%T")
                                   txt = txt .. "\n\n" .. fangh_calendar.format_today()
                                   return txt
                                end })

-- Create a wibox for each screen and add it
mywibox = {}
mypromptbox = {}
mylayoutbox = {}
mytaglist = {}
mytaglist.buttons = awful.util.table.join(
                    awful.button({ }, 1, awful.tag.viewonly),
                    awful.button({ modkey }, 1, awful.client.movetotag),
                    awful.button({ }, 3, awful.tag.viewtoggle),
                    awful.button({ modkey }, 3, awful.client.toggletag),
                    awful.button({ }, 5, function(t) awful.tag.viewnext(awful.tag.getscreen(t)) end),
                    awful.button({ }, 4, function(t) awful.tag.viewprev(awful.tag.getscreen(t)) end)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
                                                  -- Without this, the following
                                                  -- :isvisible() makes no sense
                                                  c.minimized = false
                                                  if not c:isvisible() then
                                                      awful.tag.viewonly(c:tags()[1])
                                                  end
                                                  -- This will also un-minimize
                                                  -- the client, if needed
                                                  client.focus = c
                                                  c:raise()
                                              end
                                          end),
                     awful.button({ }, 2, function (c) c:kill() end),
                     awful.button({ }, 3, function ()
                                              if instance then
                                                  instance:hide()
                                                  instance = nil
                                              else
                                                  instance = awful.menu.clients({
                                                      theme = { width = 250 }
                                                  })
                                              end
                                          end),
                     awful.button({ }, 4, function ()
                                              awful.client.focus.byidx(1)
                                              if client.focus then client.focus:raise() end
                                          end),
                     awful.button({ }, 5, function ()
                                              awful.client.focus.byidx(-1)
                                              if client.focus then client.focus:raise() end
                                          end))

-- }}}
-- {{{   Personal stuff
-- Fonctions perso
function gethost()
   local f = io.popen("/bin/hostname")
   local n = f:read("*a") or "none"
   f:close()
   return string.gsub(n, "\n$", "")
end

-- Focused client logging for stats
--require("clistats")
--clistats.init()

-- Personal helper library for things written in C
package.cpath = config_dir .. "/?.so;" .. package.cpath
require("lousy")

-- Backlight helper
local backlight_notif_id = nil
function change_backlight(mult)
   local new_bl = lousy.get_backlight() * mult
   if new_bl < 0.5 then new_bl = 0.5
   elseif new_bl > 100 then new_bl = 100 end
   lousy.set_backlight(new_bl)
   local notif = naughty.notify({
         text = "Backlight level: " .. new_bl .. "%",
         replaces_id = backlight_notif_id,
         icon = icon_theme.get("notifications", "notification-display-brightness")
   })
   backlight_notif_id = notif.id
end

-- Screen helper
function get_next_screen()
   local s = mouse.screen + 1
   if s > screen.count() then s = 1 end
   return s
end

-- Switch tag on next screen
function next_screen_viewnext()
   awful.tag.viewnext(get_next_screen())
end
function next_screen_viewprev()
   awful.tag.viewprev(get_next_screen())
end

-- {{{ Auto xrandr!
-- local connected_displays = lousy.count_connected_displays()
-- local display_timer = timer { timeout = 2.5 }
-- display_timer:connect_signal("timeout", function ()
--                                 local new_displays = lousy.count_connected_displays()
--                                 if new_displays ~= conntected_displays then
--                                    auto_set_screen("left-of")
--                                    connected_displays = new_displays
--                                 end
-- end)
-- display_timer:start()
-- }}}

-- {{{     Window management
-- Gestion de la titlebar
local all_titlebars = {}
function titlebar_add(c)
    if c.type == "normal" or c.type == "dialog" or c.type == "utility"then
        -- Widgets that are aligned to the left
        local left_layout = wibox.layout.fixed.horizontal()
        left_layout:add(awful.titlebar.widget.iconwidget(c))

        -- Widgets that are aligned to the right
        local right_layout = wibox.layout.fixed.horizontal()
        right_layout:add(awful.titlebar.widget.floatingbutton(c))
        right_layout:add(awful.titlebar.widget.maximizedbutton(c))
        right_layout:add(awful.titlebar.widget.stickybutton(c))
        right_layout:add(awful.titlebar.widget.ontopbutton(c))
        right_layout:add(awful.titlebar.widget.closebutton(c))

        -- The title goes in the middle
        local title = awful.titlebar.widget.titlewidget(c)
        title:buttons(awful.util.table.join(
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
                ))

        -- Now bring it all together
        local layout = wibox.layout.align.horizontal()
        layout:set_left(left_layout)
        layout:set_right(right_layout)
        layout:set_middle(title)

        awful.titlebar(c):set_widget(layout)
        all_titlebars[c] = true
    end
end
function titlebar_remove(c)
   awful.titlebar(c, { size = 0 })
   all_titlebars[c] = false
end
function toggle_titlebar(c)
   if all_titlebars[c] then
      titlebar_remove(c)
   else
      titlebar_add(c)
   end
end
function handle_titlebar(c)
   if awful.client.floating.get(c) and not c.fullscreen then
      if not all_titlebars[c] and not no_titlebar_apps[c.class] and not no_titlebar_apps[c.instance] then
         titlebar_add(c)
      end
   else
      if all_titlebars[c] then
         titlebar_remove(c)
      end
   end
end

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
-- }}}
-- {{{     Widgets perso
separator = wibox.widget.imagebox()
separator:set_image(config_dir .. "/icons/separator.png")

require("netmon")
ifaces = {}
if gethost() == "thor" then
   ifaces["E"] = "lan"
elseif gethost() == "odin" then
   ifaces["E"] = "eth0"
   ifaces["W"] = "wlan0"
elseif gethost() == "baldr" then
   ifaces["E"] = "enp12s0"
   ifaces["W"] = "wlp3s0"
end
net_mon = netmon.new(ifaces, "8.8.4.4")

if gethost() == "thor" then
   require("ipmon")
   ip_mon = ipmon.new({})
end

if gethost() == "thor" then
   require("nvtemp")
   nvtemp_mon = nvtemp.new()

   local icon_nv = widget({ type = "imagebox" })
   icon_nv.image = image(config_dir .. "/icons/temp.png")

   nv_w = { nvtemp_mon.widget, icon_nv, separator,
            layout = awful.widget.layout.horizontal.rightleft
   }
end

require("locksmon")
locks_mon = locksmon.new()

require("fish")
fish_w = fish.new()

-- Time-based indications using theme colors
if ifaces["W"] ~= nil then
   local function clockcheck()
      local time = tonumber(os.date("%H%M"))
      local ssid = net_mon:ssid("W")
      if ssid == nil then return end
      local at_work = ssid:lower():match("^findspire")
      local theme = beautiful.get()
      local new_normal = theme.bg_normal_ok
      local new_focus = theme.bg_focus_ok
      local new_urgent = theme.bg_urgent_ok
      if at_work and time >= 1750 then
         new_normal = theme.bg_normal_emerg
         new_focus = theme.bg_focus_emerg
         new_urgent = theme.bg_urgent_emerg
      elseif at_work and time >= 1720 then
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
   local clockwarn = timer { timeout = 60 }
   clockwarn:connect_signal("timeout", clockcheck)
   clockwarn:start()
   clockcheck()
end

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

-- Media player control
function mpris2(command)
   return function()
      os.execute("~/bin/mpris2-control " .. command .. " &")
   end
end

-- Vicious widgets
vicious.cache(vicious.widgets.cpu)
cpu_icon = wibox.widget.imagebox()
cpu_icon:set_image(config_dir .. "/icons/cpu.png")
cpu_graph = awful.widget.graph()
cpu_graph:set_width(32):set_height(18)
cpu_graph:set_background_color("#000000")
cpu_graph:set_color({type = "linear", from = {0, 0}, to = {0, 18},
                     stops = {{0, "#CC6666"}, {0.5, "#CCCC66"}, {1.0, "#66CC66"}}})
vicious.register(cpu_graph, vicious.widgets.cpu, "$1", 3)

vicious.cache(vicious.contrib.sensors)
cputemp_widget = awful.widget.progressbar()
cputemp_widget:set_width(4)
cputemp_widget:set_vertical(true)
cputemp_widget:set_background_color("#000000")
cputemp_widget:set_color({type = "linear", from = {0, 0}, to = {0, 18},
                          stops = {{0, "#CC6666"}, {0.2, "#CC66CC"}, {1.0, "#66CCCC"}}})
vicious.register(cputemp_widget, vicious.contrib.sensors,
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
mem_widget = awful.widget.progressbar()
mem_widget:set_width(8)
mem_widget:set_height(18)
mem_widget:set_vertical(true)
mem_widget:set_background_color("#000000")
mem_widget:set_color({type = "linear", from = {0, 0}, to = {0, 18},
             stops = {{0, "#CC6666"}, {0.5, "#CCCC66"}, {1.0, "#66CC66"}}})
vicious.register(mem_widget, vicious.widgets.mem, "$1", 3)
swap_widget = awful.widget.progressbar()
swap_widget:set_width(8)
swap_widget:set_height(18)
swap_widget:set_vertical(true)
swap_widget:set_background_color("#000000")
swap_widget:set_color({type = "linear", from = {0, 0}, to = {0, 18},
             stops = {{0, "#CC6666"}, {0.5, "#CC66CC"}, {1.0, "#6666CC"}}})
vicious.register(swap_widget, vicious.widgets.mem, "$5", 3)

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

vol_widget = awful.widget.progressbar()
vol_widget:set_width(10)
vol_widget:set_height(18)
vol_widget:set_vertical(true)
vol_widget:set_background_color("#000000")
vol_widget:set_border_color("#000000")
vicious.register(vol_widget, brutal.pulse,
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
      local name = sink_data["prop.application.process.name"]
         or sink_data["prop.application.process.binary"]
      local volume = 100. * sink_data.volume / 65536.
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
       awful.button({ }, 1, function () awful.util.spawn("pavucontrol") end),
       awful.button({ }, 2, volume_mute),
       awful.button({ }, 3, function () brutal.pulse.profiles_menu(volume_update):toggle() end),
       awful.button({ }, 4, volume_up),
       awful.button({ }, 5, volume_down),
       awful.button({ "Shift" }, 4, volume_music_up),
       awful.button({ "Shift" }, 5, volume_music_down)
))

-- Pomodoro widget
pomodoro.init()
-- }}}
-- {{{     Raccourcis claviers persos
local keydoc = require("keydoc")

persokeys = {
   -- Volume
   keydoc.group("Audio volume"),
   awful.key({ }, "XF86AudioRaiseVolume", volume_up),
   awful.key({ }, "XF86AudioLowerVolume", volume_down),
   awful.key({ }, "XF86AudioMute",        volume_mute),

   awful.key({ modkey }, "Up",        volume_up,   "Raise volume"),
   awful.key({ modkey }, "Down",      volume_down, "Lower volume"),
   awful.key({ modkey }, "KP_Delete", volume_mute, "Toggle mute"),

   awful.key({ "Shift" }, "XF86AudioRaiseVolume", volume_music_up),
   awful.key({ "Shift" }, "XF86AudioLowerVolume", volume_music_down),
   awful.key({ modkey, "Shift" }, "Up",           volume_music_up,   "Raise music volume"),
   awful.key({ modkey, "Shift" }, "Down",         volume_music_down, "Lower music volume"),

   -- Luminosité
   awful.key({ }, "XF86MonBrightnessUp",   function () change_backlight(1.1) end),
   awful.key({ }, "XF86MonBrightnessDown", function () change_backlight(0.9) end),

   keydoc.group("Power management"),
   -- F1 - mettre en veille
   awful.key({ modkey, "Control" }, "F1", function () awful.util.spawn("gksudo systemctl suspend") end,   "Suspend"),
   -- F2 - verrouiller l'écran
   awful.key({ }, "XF86ScreenSaver",      function () awful.util.spawn("xscreensaver-command -lock") end),
   awful.key({ modkey, "Control" }, "F2", function () awful.util.spawn("xscreensaver-command -lock") end, "Lock screen"),
   -- F3 - éteindre l'écran
   awful.key({ }, "XF86Battery",          function () awful.util.spawn("xset dpms force suspend") end),
   awful.key({ modkey, "Control" }, "F3", function () awful.util.spawn("xset dpms force suspend") end,    "Turn screen off"),

   -- Caps Lock et Num Lock
   awful.key({ }, "Num_Lock",  function() locks_mon:update("Num_Lock")  end),
   awful.key({ }, "Caps_Lock", function() locks_mon:update("Caps_Lock") end),

   -- Touches tag suivant/précédent au-dessus du pavé numérique
   awful.key({ }, "XF86Back",     awful.tag.viewprev),
   awful.key({ }, "XF86Forward" , awful.tag.viewnext),

   -- Tag suivant/précédent sur l'autre écran avec PageUp/PageDown
   keydoc.group("Tag navigation"),
   awful.key({ modkey }, "Prior", next_screen_viewprev, "Next screen: Previous tag"),
   awful.key({ modkey }, "Next",  next_screen_viewnext, "Next screen: Next tag"),

   -- Éditeur de texte avec la touche ThinkVantage
   awful.key({                 }, "XF86Launch1", function () awful.util.spawn(editor_cmd) end),
   awful.key({ modkey          }, "KP_Insert",   function () awful.util.spawn(editor_cmd) end),
   awful.key({ modkey, "Shift" }, "Return",      function () awful.util.spawn(editor_cmd) end),

   -- Media player
   keydoc.group("Media player"),
   awful.key({ }, "XF86AudioStop", mpris2("stop")),
   awful.key({ }, "XF86AudioPlay", mpris2("playpause")),
   awful.key({ }, "XF86AudioPrev", mpris2("prev")),
   awful.key({ }, "XF86AudioNext", mpris2("next")),

   awful.key({ modkey, "Control" }, "Up",    mpris2("stop"),      "Stop"),
   awful.key({ modkey, "Control" }, "Down",  mpris2("playpause"), "Play/Pause"),
   awful.key({ modkey, "Control" }, "Left",  mpris2("prev"),      "Previous"),
   awful.key({ modkey, "Control" }, "Right", mpris2("next"),      "Next"),

   awful.key({ modkey, "Shift"   }, "d", function () awful.util.spawn("dspop") end, "Run dspop"),
   awful.key({ modkey, "Control" }, "d", function () awful.util.spawn("dmpd") end,  "Run dmpd"),
   awful.key({ modkey, "Shift"   }, "i", mpris2("info"),                            "Show MPRIS2 info"),
   awful.key({ modkey, "Shift"   }, "o", mpris2("nextplayer"),                      "Switch to the next MPRIS2 player"),

   -- Misc
   keydoc.group("Misc"),
   awful.key({ modkey          }, "Print", screenshot,                                 "Take a screenshot with Shutter"),
   awful.key({ modkey, "Shift" }, "f",     function() awful.mouse.finder():find() end, "Locate pointer"),
   awful.key({ modkey, "Shift" }, "w",     function () change_wallpapers(true) end,    "Change wallpaper"),
   awful.key({ modkey          }, "F1",    keydoc.display),
}

persoclientkeys = {
   keydoc.group("Client controls"),
   awful.key({ modkey, "Shift"   }, "t",           toggle_titlebar,                              "Toggle titlebar"),
   awful.key({ modkey            }, "s",           function (c) c.sticky = not c.sticky end,     "Toggle sticky"),
   awful.key({ modkey, "Control" }, "i",           win_info,                                     "Show info"),
   awful.key({ modkey            }, "KP_Add",      function (c) resize_client_ratio(1.1, c) end, "Increase size by 10%"),
   awful.key({ modkey            }, "KP_Subtract", function (c) resize_client_ratio(0.9, c) end, "Decrease size by 10%"),
   awful.key({ modkey, "Control" }, "KP_Add",      function (c) resize_client_ratio(1.5, c) end, "Increase size by 50%"),
   awful.key({ modkey, "Control" }, "KP_Subtract", function (c) resize_client_ratio(0.5, c) end, "Decrease size by 50%"),
}
-- }}}
-- }}}
-- {{{   Wibox creation
local wibox_created = false
function create_wibox()
   for s = 1, screen.count() do
      -- Create a promptbox for each screen
      mypromptbox[s] = awful.widget.prompt()
      -- Create an imagebox widget which will contains an icon indicating which layout we're using.
      -- We need one layoutbox per screen.
      mylayoutbox[s] = awful.widget.layoutbox(s)
      mylayoutbox[s]:buttons(awful.util.table.join(
                                awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                                awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                                awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                                awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
      -- Create a taglist widget
      mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.filter.all, mytaglist.buttons)

      -- Create a tasklist widget
      mytasklist[s] = awful.widget.tasklist(s, awful.widget.tasklist.filter.currenttags, mytasklist.buttons)

      -- Create the wibox
      mywibox[s] = awful.wibox({ position = "top", height = "18", screen = s, ontop = nil })

      -- Widgets that are aligned to the left
      local left_layout = wibox.layout.fixed.horizontal()
      left_layout:add(mylauncher)
      left_layout:add(mytaglist[s])
      left_layout:add(mypromptbox[s])

      local my_right_widgets = _.concat({
            separator,
            tb_mails, tb_msmtpq, nv_w}, separator,
         pomodoro.icon_widget, fish_w.widget, separator,
         cpu_icon, cpu_graph, cputemp_widget, mem_icon, mem_widget, swap_widget, separator,
         net_mon.widget, {ip_mon and ip_mon.widget or nil}, bat_widget, vol_widget, separator, locks_mon.widget
      )

      -- Widgets that are aligned to the right
      local right_layout = wibox.layout.fixed.horizontal()
      for i, w in pairs(my_right_widgets) do right_layout:add(w) end
      if s == 1 then right_layout:add(separator) end

      if s == 1 then right_layout:add(wibox.widget.systray()) end
      right_layout:add(mytextclock_icon)
      right_layout:add(mytextclock)
      right_layout:add(mylayoutbox[s])

      -- Now bring it all together (with the tasklist in the middle)
      local layout = wibox.layout.align.horizontal()
      layout:set_left(left_layout)
      layout:set_middle(mytasklist[s])
      layout:set_right(right_layout)

      mywibox[s]:set_widget(layout)
   end
   wibox_created = true
end
create_wibox()
-- }}}
-- }}}
-- {{{ Mouse bindings
root.buttons(awful.util.table.join(
    awful.button({ }, 3, function () mymainmenu:toggle() end),
    awful.button({ }, 5, awful.tag.viewnext),
    awful.button({ }, 4, awful.tag.viewprev)
))
-- }}}

-- {{{ Key bindings
globalkeys = awful.util.table.join(
    keydoc.group("Tag navigation"),
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       , "Previous tag"),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       , "Next tag"),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore, "Jump to most recent tag"),

    keydoc.group("Client navigation"),
    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end,
        "Next"),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end,
        "Previous"),

    keydoc.group("Menu"),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end,    "Show main menu"),
    awful.key({ modkey, "Control" }, "w", function () mylayoutsmenu:show() end, "Show layouts menu"),

    -- Layout manipulation
    keydoc.group("Layout manipulation"),
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end, "Swap with next client"),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end, "Swap with previous client"),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end, "Focus next screen"),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end, "Focus previous screen"),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto,                      "Jump to urgent client", "Client navigation"),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end,
        "Jump to most recent client", "Client navigation"),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end, "Increase master width"),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end, "Decrease master width"),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end, "Increase master number"),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end, "Decrease master number"),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end, "Add a column"),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end, "Remove a column"),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end, "Next layout"),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end, "Previous layout"),

    awful.key({ modkey, "Control" }, "n", awful.client.restore, "Restore a minimized client", "Client navigation"),

    -- Prompt
    awful.key({ modkey },            "r",     function () mypromptbox[mouse.screen]:run() end),

    awful.key({ modkey }, "x",
              function ()
                  awful.prompt.run({ prompt = "Run Lua code: " },
                  mypromptbox[mouse.screen].widget,
                  awful.util.eval, nil,
                  awful.util.getdir("cache") .. "/history_eval")
              end),
    -- Menubar
    awful.key({ modkey }, "p", function() menubar.show() end)
)

clientkeys = awful.util.table.join(
    keydoc.group("Client controls"),
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end, "Toggle fullscreen"),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end, "Kill"),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     , "Toggle floating"),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end, "Swap with master"),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        , "Move to other screen"),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end, "Redraw"),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end, "Toggle on-top"),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end,
        "Minimize"),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end,
        "Maximize")
)

-- Compute the maximum number of digit we need, limited to 9
keynumber = 0
for s = 1, screen.count() do
   keynumber = math.min(9, math.max(#tags[s], keynumber));
end

-- Bind all key numbers to tags.
-- Be careful: we use keycodes to make it works on any keyboard layout.
-- This should map on the top row of your keyboard, usually 1 to 9.
for i = 1, keynumber do
    globalkeys = awful.util.table.join(globalkeys,
        -- View tag only.
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        local tag = awful.tag.gettags(screen)[i]
                        if tag then
                           awful.tag.viewonly(tag)
                        end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      local tag = awful.tag.gettags(screen)[i]
                      if tag then
                         awful.tag.viewtoggle(tag)
                      end
                  end),
        -- Move client to tag.
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.movetotag(tag)
                          end
                     end
                  end),
        -- Toggle tag.
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus then
                          local tag = awful.tag.gettags(client.focus.screen)[i]
                          if tag then
                              awful.client.toggletag(tag)
                          end
                      end
                  end))
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
                     buttons = clientbuttons } },
    -- Simple rules for floating windows
    { rule_any = { class = { "BBQScreenClient2", "Galculator", "Gimp", "Gmpc",
                             "mpv",
                             "pinentry", "Plugin-container",
                             "Smplayer", "VirtualBox", "Vlc",
                             "Wine" },
                   instance = { "crx_eggkanocgddhmamlbiijnphhppkpkmkl", "pinentry-gtk-2", "popcorntime", "wpa_gui" },
                   name = { "Android Device Monitor", "Gnuplot (window id : 0)", "Minecraft", "Popcorn Time",
                            "R Graphics: Device 2 (ACTIVE)" } },
      properties = { floating = true } },

    { rule = { class = "Firefox" },
      except = { instance = "Navigator" },
      properties = { floating = true } },
    { rule = { class = "Thunderbird" },
      properties = { tag = tags[screen.count()][screen.count() > 1 and 1 or 2] } },
    { rule = { class = "Gajim.py", role = "roster" },
      properties = { tag = tags[screen.count()][1] } },
    { rule = { class = "Gajim.py", role = "messages" },
      callback = awful.client.setslave },
    { rule_any = { instance = { "spotify.exe", "spotify", "Steam" } },
      properties = { floating = true, tag = tags[1][9] } },
    { rule = { instance = "popcorntime" },
      properties = { floating = true, tag = tags[screen.count()][9] } },
    { rule = { class = "Audacious" },
      properties = { floating = true, ontop = true, sticky = true } },
    { rule = { instance = "pluginloader.exe" },
      properties = { floating = true, fullscreen = true } },
    { rule_any = { class = {"Arandr", "Pavucontrol" } },
      properties = { floating = true },
      callback = awful.placement.centered },
    { rule = { class = "Skype", role = "CallWindow" },
      properties = { floating = true },
      callback = awful.placement.centered },
}
no_titlebar_apps = {
   ["gnome-commander"] = true,
   ["lt-gnome-commander"] = true,
   ["plugin-container"] = true,
   ["pluginloader.exe"] = true,
   ["Steam"] = true,
   ["Wine"] = true,
   ["xine"] = true,
   ["Xitk"] = true,
}
-- }}}

-- {{{ Signals
-- Signal function to execute when a new client appears.
client.connect_signal("manage", function (c, startup)
    -- Enable sloppy focus
    c:connect_signal("mouse::enter", function(c)
        if awful.layout.get(c.screen) ~= awful.layout.suit.magnifier
            and awful.client.focus.filter(c) then
            client.focus = get_transient(c)
        end
    end)
    new_transient(c)

    if not startup then
        -- Set the windows at the slave,
        -- i.e. put it at the end of others instead of setting it master.
        awful.client.setslave(c)

        -- Put windows in a smart way, only if they does not set an initial position.
        if not c.size_hints.user_position and not c.size_hints.program_position then
            awful.placement.no_overlap(c)
            awful.placement.no_offscreen(c)
        end
    end

    -- Add a titlebar
    handle_titlebar(c)
    c:connect_signal("property::floating", handle_titlebar)
end)

client.connect_signal("focus", function(c)
    c.border_color = beautiful.border_focus
    --clistats.focus(c)
end)
client.connect_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}
-- {{{ Timers
if nvtemp_mon then
   mytimer5 = timer { timeout = 5 }
   mytimer5:connect_signal("timeout", function ()
                              nvtemp_mon:update()
   end)
   mytimer5:start()
end

mytimer15 = timer { timeout = 15 }
mytimer15:connect_signal("timeout", function ()
    net_mon:update()
    if ip_mon then ip_mon:update() end
    tb_mails_update()
    tb_msmtpq_update()

    --clistats.idle(lousy.idle())
end)
mytimer15:start()

mytimer300 = timer { timeout = 300 }
mytimer300:connect_signal("timeout", function () change_wallpapers() end)
mytimer300:start()
-- }}}
-- {{{ Disable startup-notification
-- http://awesome.naquadah.org/wiki/Disable_startup-notification_globally
-- local oldspawn1 = awful.util.spawn
-- awful.util.spawn = function (s)
--   oldspawn1(s, false)
-- end
-- }}}
-- {{{ Run applications in a different (transient) systemd scope
-- This allows awesome to be restarted by systemd without killing the
-- applications started from it.
local oldspawn2 = awful.util.spawn
awful.util.spawn = function (cmd, sn, props)
   local params = ""
   if props ~= nil then
      local k, v
      for k, v in pairs(props) do
         params = params .. "-p " .. k .. "=" .. v .. " "
      end
   end
   cmd = "systemd-run --user --scope " .. params .. cmd
   -- naughty.notify({ text = cmd })
   oldspawn2(cmd, sn)
end
-- }}}
