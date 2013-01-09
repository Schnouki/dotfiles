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

-- Eminent dynamic tagging
require("eminent")
-- Vicious widgets
vicious = require("vicious")
vicious.contrib = require("vicious.contrib")
-- Markup functions
require("markup")

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
editor = "emacsclient -c -a \"\""
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
if beautiful.wallpaper then
    for s = 1, screen.count() do
        gears.wallpaper.maximized(beautiful.wallpaper, s, true)
    end
end
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
awful.tag.setmwfact(0.72, tags[1][1])
awful.tag.setmwfact(0.72, tags[1][2])
awful.tag.setmwfact(0.65, tags[1][3])
awful.tag.setmwfact(0.65, tags[1][8])

-- Set last tag of each screen to floating
for s = 1, screen.count() do
   awful.layout.set(layouts[8], tags[s][9])
end
-- }}}

-- {{{ Menu
-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

gamemenu = {
   { "battle for wesnoth", "wesnoth", "/usr/share/icons/wesnoth-icon.png" },
   { "frozen bubble", "frozen-bubble", ("/usr/share/pixmaps/frozen-bubble.png") },
   { "gplanarity", "gplanarity", ("/usr/share/pixmaps/gplanarity.png") },
   { "hex-a-hop", "hex-a-hop", ("/usr/share/hex-a-hop/icon.bmp") },
   { "kildclient", "kildclient", ("/usr/share/pixmaps/kildclient.png") },
   { "kobo deluxe", "kobodl", ("/usr/share/pixmaps/kobo-icon.xpm") },
   { "minecraft", "minecraft", ("/usr/share/pixmaps/minecraft.png") },
   { "naev", "naev", ("/usr/share/pixmaps/naev.png") },
   { "torchlight", "torchlight", ("/usr/share/pixmaps/torchlight.png") },
}

utilsmenu = {
   { "galculator", "galculator", ("/usr/share/icons/hicolor/48x48/apps/galculator.png") },
   { "qalculate-gtk", "qalculate", ("/usr/share/pixmaps/qalculate.png") },
   { "gdmap", "gdmap", ("/usr/share/pixmaps/gdmap_icon.png") },
   { "gucharmap", "gucharmap", ("/usr/share/icons/gnome/16x16/apps/accessories-character-map.png") },
   { "pavucontrol", "pavucontrol", ("/usr/share/icons/gnome/16x16/apps/multimedia-volume-control.png") },
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "jeux", gamemenu },
                                    { "utils", utilsmenu },
                                    { "gajim", "gajim", ("/usr/share/icons/hicolor/64x64/apps/gajim.png") },
                                    { "firefox", "firefox", ("/usr/share/icons/hicolor/16x16/apps/firefox.png") },
                                    { "chromium", "chromium", ("/usr/share/icons/hicolor/16x16/apps/chromium.png") },
                                    { "shotwell", "shotwell", (config_dir .. "/icons/shotwell.png") },
                                    { "spotify", "spotify", (config_dir .. "/icons/spotify.png") },
                                    { "libre office", "soffice", ("/usr/share/icons/hicolor/16x16/apps/libreoffice-writer.png") },
                                    { "gcmd", "gnome-commander", ("/usr/share/pixmaps/gnome-commander.png") },
                                    { "open terminal", terminal }
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
mytextclock = awful.widget.textclock("%H:%M")
mytextclock_t = awful.tooltip({ objects = { mytextclock },
                                timer_function = function() return os.date("%A %e %B %Y\n%T") end })

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
                                                  instance = awful.menu.clients({ width=250 })
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

-- {{{     Window management
-- Gestion de la titlebar
local all_titlebars = {}
function titlebar_add(c)
    if c.type == "normal" or c.type == "dialog" then
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
   if awful.client.floating.get(c) then
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
end
net_mon = netmon.new(ifaces, "8.8.8.8")

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

require("battmon")
batt_mon = battmon.new()

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

-- Localiser le pointeur de la souris
mymousefinder = awful.mouse.finder()

-- Widget with number of unread mails if notmuch is available
local f = io.open("/usr/bin/notmuch")
if f then
   tb_mails = wibox.widget.textbox()
   tb_mails_color_normal   = "#7cb8bb" -- blue-1
   tb_mails_color_updating = "#ac7373" -- red-2
   tb_mails_color = tb_mails_color_normal
   function tb_mails_set_count(n)
      local s = markup.fg.color(tb_mails_color, "✉ " .. n)
      if n > 0 then
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
else
   -- Stubs for stuff needed elsewhere
   tb_mails = nil
   function tb_mails_update() end
   function tb_mails_set_count(n) end
   function tb_mails_updating(u) end
end

-- Media player control
function mpris2(command)
   return function()
      os.execute("~/bin/mpris2-control " .. command .. " &")
   end
end

-- Vicious widgets
cpu_mem_gradient = "linear:0,18:0,0:0,#66CC66:0.5,#CCCC66:1,#CC6666"

cpuicon = wibox.widget.imagebox()
cpuicon:set_image(config_dir .. "/icons/cpu.png")
cpuwidget = awful.widget.graph({ height = 18, width = 50 })
cpuwidget:set_background_color("#000000")
cpuwidget:set_border_color("#000000")
cpuwidget:set_color(cpu_mem_gradient)
vicious.register(cpuwidget, vicious.widgets.cpu, "$1", 3)

memicon = wibox.widget.imagebox()
memicon:set_image(config_dir .. "/icons/mem.png")
memwidget = awful.widget.graph({ height = 18, width = 50 })
memwidget:set_background_color("#000000")
memwidget:set_border_color("#000000")
memwidget:set_color(cpu_mem_gradient)
vicious.register(memwidget, vicious.widgets.mem, "$1", 3)

volbar = awful.widget.progressbar()
volbar:set_width(10)
volbar:set_height(18)
volbar:set_vertical(true)
volbar:set_background_color("#000000")
volbar:set_border_color("#000000")
vicious.register(volbar, vicious.contrib.pulse,
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

function volume_up()   vicious.contrib.pulse.add( 5)  vicious.force({volbar}) end
function volume_down() vicious.contrib.pulse.add(-5)  vicious.force({volbar}) end
function volume_mute() vicious.contrib.pulse.toggle() vicious.force({volbar}) end

volbar:buttons(awful.util.table.join(
       awful.button({ }, 1, function () awful.util.spawn("pavucontrol") end),
       awful.button({ }, 4, volume_up),
       awful.button({ }, 5, volume_down),
       awful.button({ }, 2, volume_mute)
))

-- }}}
-- {{{     Raccourcis claviers persos
persokeys = {
   -- Volume
   awful.key({ }, "XF86AudioRaiseVolume", volume_up),
   awful.key({ }, "XF86AudioLowerVolume", volume_down),
   awful.key({ }, "XF86AudioMute",        volume_mute),

   awful.key({ modkey }, "Up",        volume_up),
   awful.key({ modkey }, "Down",      volume_down),
   awful.key({ modkey }, "KP_Delete", volume_mute),

   -- F2 - verrouiller l'écran
   awful.key({ }, "XF86ScreenSaver",      function () awful.util.spawn("xscreensaver-command -lock") end),
   awful.key({ modkey, "Control" }, "F2", function () awful.util.spawn("xscreensaver-command -lock") end),
   -- F3 - éteindre l'écran
   awful.key({ }, "XF86Battery",          function () awful.util.spawn("xset dpms force suspend") end),
   awful.key({ modkey, "Control" }, "F3", function () awful.util.spawn("xset dpms force suspend") end),
   -- F7 - écran externe
   awful.key({ }, "XF86Display",     function () awful.util.spawn("gdisper") end),

   -- Touches tag suivant/précédent au-dessus du pavé numérique
   awful.key({ }, "XF86Back",     awful.tag.viewprev),
   awful.key({ }, "XF86Forward" , awful.tag.viewnext),

   -- Impr. Écran
   awful.key({ }, "Print", function () awful.util.spawn("twitscreen") end),

   -- Éditeur de texte avec la touche ThinkVantage
   awful.key({                 }, "XF86Launch1", function () awful.util.spawn(editor_cmd) end),
   awful.key({ modkey          }, "KP_Insert",   function () awful.util.spawn(editor_cmd) end),
   awful.key({ modkey, "Shift" }, "Return",      function () awful.util.spawn(editor_cmd) end),

   -- Localiser le pointeur
   awful.key({ modkey, "Shift" }, "f", function() mymousefinder:find() end),

   -- Media player
   awful.key({ }, "XF86AudioStop", mpris2("stop")),
   awful.key({ }, "XF86AudioPlay", mpris2("playpause")),
   awful.key({ }, "XF86AudioPrev", mpris2("prev")),
   awful.key({ }, "XF86AudioNext", mpris2("next")),

   awful.key({ modkey, "Control" }, "Up",    mpris2("stop")),
   awful.key({ modkey, "Control" }, "Down",  mpris2("playpause")),
   awful.key({ modkey, "Control" }, "Left",  mpris2("prev")),
   awful.key({ modkey, "Control" }, "Right", mpris2("next")),

   awful.key({ modkey, "Shift"   }, "d", function () awful.util.spawn("dspop") end),
   awful.key({ modkey, "Shift"   }, "i", mpris2("info")),

   -- TypeMatrix bépo
   awful.key({ modkey            }, "i", awful.tag.history.restore), -- "desktop" key
   awful.key({                   }, "XF86Calculator", function () awful.tag.viewonly(tags[1][1]) end),
   awful.key({                   }, "XF86Mail",       function () awful.tag.viewonly(tags[1][2]) end),
   awful.key({                   }, "XF86HomePage",   function () awful.tag.viewonly(tags[1][3]) end),
   awful.key({         "Shift"   }, "XF86HomePage",   function () awful.tag.viewonly(tags[1][8]) end),
   awful.key({                   }, "XF86AudioPause", function () awful.tag.viewonly(tags[1][9]) end), -- Shift+Play
}

persoclientkeys = {
   awful.key({ modkey, "Shift"   }, "t", toggle_titlebar),
   awful.key({ modkey            }, "s", function (c) c.sticky = not c.sticky end),
   awful.key({ modkey, "Shift"   }, "s", function (c) c.ontop = not c.ontop end),
   awful.key({ modkey, "Control" }, "i", win_info),
}
-- }}}
-- }}}
-- {{{   Wibox creation
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

    local my_right_widgets = {
       separator,
       tb_mails, nv_w, separator,
       cpuicon, cpuwidget, memicon, memwidget, separator,
       net_mon.widget, ip_mon and ip_mon.widget or nil, batt_mon.widget, volbar, separator
    }

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
    awful.key({ modkey,           }, "Left",   awful.tag.viewprev       ),
    awful.key({ modkey,           }, "Right",  awful.tag.viewnext       ),
    awful.key({ modkey,           }, "Escape", awful.tag.history.restore),

    awful.key({ modkey,           }, "j",
        function ()
            awful.client.focus.byidx( 1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "k",
        function ()
            awful.client.focus.byidx(-1)
            if client.focus then client.focus:raise() end
        end),
    awful.key({ modkey,           }, "w", function () mymainmenu:show() end),

    -- Layout manipulation
    awful.key({ modkey, "Shift"   }, "j", function () awful.client.swap.byidx(  1)    end),
    awful.key({ modkey, "Shift"   }, "k", function () awful.client.swap.byidx( -1)    end),
    awful.key({ modkey, "Control" }, "j", function () awful.screen.focus_relative( 1) end),
    awful.key({ modkey, "Control" }, "k", function () awful.screen.focus_relative(-1) end),
    awful.key({ modkey,           }, "u", awful.client.urgent.jumpto),
    awful.key({ modkey,           }, "Tab",
        function ()
            awful.client.focus.history.previous()
            if client.focus then
                client.focus:raise()
            end
        end),

    -- Standard program
    awful.key({ modkey,           }, "Return", function () awful.util.spawn(terminal) end),
    awful.key({ modkey, "Control" }, "r", awesome.restart),
    awful.key({ modkey, "Shift"   }, "q", awesome.quit),

    awful.key({ modkey,           }, "l",     function () awful.tag.incmwfact( 0.05)    end),
    awful.key({ modkey,           }, "h",     function () awful.tag.incmwfact(-0.05)    end),
    awful.key({ modkey, "Shift"   }, "h",     function () awful.tag.incnmaster( 1)      end),
    awful.key({ modkey, "Shift"   }, "l",     function () awful.tag.incnmaster(-1)      end),
    awful.key({ modkey, "Control" }, "h",     function () awful.tag.incncol( 1)         end),
    awful.key({ modkey, "Control" }, "l",     function () awful.tag.incncol(-1)         end),
    awful.key({ modkey,           }, "space", function () awful.layout.inc(layouts,  1) end),
    awful.key({ modkey, "Shift"   }, "space", function () awful.layout.inc(layouts, -1) end),

    awful.key({ modkey, "Control" }, "n", awful.client.restore),

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
    awful.key({ modkey,           }, "f",      function (c) c.fullscreen = not c.fullscreen  end),
    awful.key({ modkey, "Shift"   }, "c",      function (c) c:kill()                         end),
    awful.key({ modkey, "Control" }, "space",  awful.client.floating.toggle                     ),
    awful.key({ modkey, "Control" }, "Return", function (c) c:swap(awful.client.getmaster()) end),
    awful.key({ modkey,           }, "o",      awful.client.movetoscreen                        ),
    awful.key({ modkey, "Shift"   }, "r",      function (c) c:redraw()                       end),
    awful.key({ modkey,           }, "t",      function (c) c.ontop = not c.ontop            end),
    awful.key({ modkey,           }, "n",
        function (c)
            -- The client currently has the input focus, so it cannot be
            -- minimized, since minimized clients can't have the focus.
            c.minimized = true
        end),
    awful.key({ modkey,           }, "m",
        function (c)
            c.maximized_horizontal = not c.maximized_horizontal
            c.maximized_vertical   = not c.maximized_vertical
        end)
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
        awful.key({ modkey }, "#" .. i + 9,
                  function ()
                        local screen = mouse.screen
                        if tags[screen][i] then
                            awful.tag.viewonly(tags[screen][i])
                        end
                  end),
        awful.key({ modkey, "Control" }, "#" .. i + 9,
                  function ()
                      local screen = mouse.screen
                      if tags[screen][i] then
                          awful.tag.viewtoggle(tags[screen][i])
                      end
                  end),
        awful.key({ modkey, "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.movetotag(tags[client.focus.screen][i])
                      end
                  end),
        awful.key({ modkey, "Control", "Shift" }, "#" .. i + 9,
                  function ()
                      if client.focus and tags[client.focus.screen][i] then
                          awful.client.toggletag(tags[client.focus.screen][i])
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
awful.rules.rules = {
    -- All clients will match this rule.
    { rule = { },
      properties = { border_width = beautiful.border_width,
                     border_color = beautiful.border_normal,
                     focus = true,
                     keys = clientkeys,
                     buttons = clientbuttons } },
    -- Simple rules for floating windows
    { rule_any = { class = { "Galculator", "Gimp", "Gmpc", "Gnote", "Klavaro", "MPlayer", "mplayer2",
                             "pinentry", "Plugin-container", "Qalculate", "Smplayer", "VirtualBox", "Vlc",
                             "Wine", "Xfmedia", "xine", "XVroot" },
                   instance = { "pinentry-gtk-2" },
                   name = { "Gnuplot (window id : 0)", "Minecraft", "R Graphics: Device 2 (ACTIVE)" } },
      properties = { floating = true } },

    { rule = { class = "Firefox" },
      except = { instance = "Navigator" },
      properties = { floating = true } },
    { rule = { class = "Gajim.py", role = "roster" },
      properties = { tag = tags[1][1] } },
    { rule = { class = "Gajim.py", role = "messages" },
      callback = awful.client.setslave },
    { rule_any = { instance = { "spotify.exe", "spotify" } },
      properties = { floating=true, tag = tags[1][9] } },
    { rule = { class = "Audacious" },
      properties = { floating = true, ontop = true, sticky = true } },
    { rule_any = { class = {"Arandr", "Pavucontrol" } },
      properties = { floating = true },
      callback = awful.placement.centered },
}
no_titlebar_apps = {
   ["gnome-commander"] = true,
   ["lt-gnome-commander"] = true,
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
mytimer5 = timer { timeout = 5 }
mytimer5:connect_signal("timeout", function ()
    batt_mon:update()
    if nvtemp_mon then nvtemp_mon:update() end
end)
mytimer5:start()

mytimer15 = timer { timeout = 15 }
mytimer15:connect_signal("timeout", function ()
    net_mon:update()
    if ip_mon then ip_mon:update() end
    tb_mails_update()

    --clistats.idle(lousy.idle())
end)
mytimer15:start()
-- }}}
-- {{{ Disable startup-notification
-- http://awesome.naquadah.org/wiki/Disable_startup-notification_globally
local oldspawn = awful.util.spawn
awful.util.spawn = function (s)
  oldspawn(s, false)
end
-- }}}
