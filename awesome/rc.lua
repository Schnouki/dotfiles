-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")
-- Eminent dynamic tagging
require("eminent")
-- Vicious widgets
require("vicious")
require("vicious.contrib")
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
    awesome.add_signal("debug::error", function (err)
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
config_dir = awful.util.getdir("config")
beautiful.init(config_dir .. "/themes/schnouki-zenburn.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvtc"
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
layouts =
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
   { "change wallpaper", beautiful.wallpaper_cmd[1] },
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awesome.conffile },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

gamemenu = {
   { "battle for wesnoth", "wesnoth", image("/usr/share/icons/wesnoth-icon.png") },
   { "frozen bubble", "frozen-bubble", image("/usr/share/pixmaps/frozen-bubble.png") },
   { "gplanarity", "gplanarity", image("/usr/share/pixmaps/gplanarity.png") },
   { "hex-a-hop", "hex-a-hop", image("/usr/share/hex-a-hop/icon.bmp") },
   { "kildclient", "kildclient", image("/usr/share/pixmaps/kildclient.png") },
   { "kobo deluxe", "kobodl", image("/usr/share/pixmaps/kobo-icon.xpm") },
   { "minecraft", "minecraft", image("/usr/share/pixmaps/minecraft.png") },
   { "naev", "naev", image("/usr/share/pixmaps/naev.png") },
}

utilsmenu = {
   { "galculator", "galculator", image("/usr/share/pixmaps/galculator.png") },
   { "gdmap", "gdmap", image("/usr/share/pixmaps/gdmap_icon.png") },
   { "gucharmap", "gucharmap", image("/usr/share/icons/gnome/16x16/apps/accessories-character-map.png") },
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "jeux", gamemenu },
                                    { "utils", utilsmenu },
                                    { "gajim", "gajim", image("/usr/share/icons/hicolor/64x64/apps/gajim.png") },
                                    { "firefox", "firefox-beta-bin", image("/usr/share/pixmaps/firefox-beta-bin-icon.png") },
                                    { "chromium", "chromium", image("/usr/share/icons/hicolor/16x16/apps/chromium.png") },
                                    { "shotwell", "shotwell", image(config_dir .. "/icons/shotwell.png") },
                                    { "spotify", "spotify", image(config_dir .. "/icons/spotify.png") },
                                    { "libre office", "soffice", image("/usr/share/icons/hicolor/16x16/apps/libreoffice-writer.png") },
                                    { "acroread", "acroread", image("/usr/share/pixmaps/acroread.png") },
                                    { "gcmd", "gnome-commander", image("/usr/share/pixmaps/gnome-commander.png") },
                                    { "open terminal", terminal }
                                  }
                        })

mylauncher = awful.widget.launcher({ image = image(beautiful.awesome_icon),
                                     menu = mymainmenu })
-- }}}

-- {{{ Wibox
-- {{{ Default stuff
-- Create a textclock widget
mytextclock_icon = widget({ type = "imagebox" })
mytextclock_icon.image = image(config_dir .. "/icons/time.png")
mytextclock = awful.widget.textclock({ align = "right" }, "%H:%M")
mytextclock_t = awful.tooltip({ objects = { mytextclock },
                                timer_function = function() return os.date("%A %e %B %Y\n%T") end })

-- Create a systray
mysystray = widget({ type = "systray" })

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
                    awful.button({ }, 5, awful.tag.viewnext),
                    awful.button({ }, 4, awful.tag.viewprev)
                    )
mytasklist = {}
mytasklist.buttons = awful.util.table.join(
                     awful.button({ }, 1, function (c)
                                              if c == client.focus then
                                                  c.minimized = true
                                              else
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
-- {{{ Personal stuff
-- Fonctions perso
function gethost()
   local f = io.popen("/bin/hostname")
   local n = f:read("*a") or "none"
   f:close()
   return string.gsub(n, "\n$", "")
end

-- {{{ Window management
-- Gestion de la titlebar
function handle_titlebar(c)
   if awful.client.floating.get(c) then
      if not c.titlebar and not no_titlebar_apps[c.class] and not no_titlebar_apps[c.instance] then
         awful.titlebar.add(c, { modkey = modkey })
      end
   else
      if c.titlebar then
         awful.titlebar.remove(c)
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

-- {{{ Widgets perso
separator = widget({ type = "imagebox" })
separator.image = image(config_dir .. "/icons/separator.png")

require("netmon")
netmon.init()
tb_net = widget({ type = "textbox" })
nm_ifs = {}
if gethost() == "thor" then
   nm_ifs["E"] = "lan"
elseif gethost() == "odin" then
   nm_ifs["E"] = "eth0"
   nm_ifs["W"] = "wlan0"
end
tb_net.text = " " .. netmon.netmon(nm_ifs, "8.8.8.8")

if gethost() == "thor" then
   require("nvtemp")
   nvtemp.init()

   sep_nv = separator
   icon_nv = widget({ type = "imagebox" })
   icon_nv.image = image(config_dir .. "/icons/temp.png")
   tb_nv = widget({ type = "textbox" })
   tb_nv.text = "..."
end

require("battmon")
tb_batt = widget({ type = "textbox" })
tb_batt.text = " " .. battery_mon() .. " "

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

   tb_mails = widget({ type = "textbox" })
   tb_mails_color_normal   = "#7cb8bb" -- blue-1
   tb_mails_color_updating = "#ac7373" -- red-2
   tb_mails_color = tb_mails_color_normal
   function tb_mails_set_count(n)
      local s = markup.fg.color(tb_mails_color, "✉ " .. n)
      if n > 0 then
         s = markup.bold(s)
      end
      tb_mails.text = s
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
cpuicon = widget({ type = "imagebox" })
cpuicon.image = image(config_dir .. "/icons/cpu.png")
cpuwidget = awful.widget.graph()
cpuwidget:set_width(50)
cpuwidget:set_background_color("#000000")
cpuwidget:set_border_color("#000000")
cpuwidget:set_gradient_colors({ "#66CC66", "#CCCC66", "#CC6666" })
cpuwidget:set_gradient_angle(180)
vicious.register(cpuwidget, vicious.widgets.cpu, "$1", 3)

memicon = widget({ type = "imagebox" })
memicon.image = image(config_dir .. "/icons/mem.png")
memwidget = awful.widget.graph()
memwidget:set_width(50)
memwidget:set_background_color("#000000")
memwidget:set_border_color("#000000")
memwidget:set_gradient_colors({ "#66CC66", "#CCCC66", "#CC6666" })
memwidget:set_gradient_angle(180)
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

volbar.widget:buttons(awful.util.table.join(
       awful.button({ }, 1, function () awful.util.spawn("pavucontrol") end),
       awful.button({ }, 4, volume_up),
       awful.button({ }, 5, volume_down),
       awful.button({ }, 2, volume_mute)
))

-- }}}

-- {{{ Raccourcis claviers persos
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
   -- F4 - mettre l'ordi en veille
   awful.key({ }, "XF86Sleep",            function () awful.util.spawn("my-s2ram") end),
   awful.key({ modkey, "Control" }, "F4", function () awful.util.spawn("my-s2ram") end),
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
   awful.key({ modkey, "Shift"   }, "t", function (c) if c.titlebar then awful.titlebar.remove(c) else awful.titlebar.add(c, { modkey = modkey }) end end),
   awful.key({ modkey            }, "s", function (c) c.sticky = not c.sticky end),
   awful.key({ modkey, "Shift"   }, "s", function (c) c.ontop = not c.ontop end),
   awful.key({ modkey, "Control" }, "i", win_info),
}
-- }}}

-- {{{ Wibox creation
for s = 1, screen.count() do
    -- Create a promptbox for each screen
    mypromptbox[s] = awful.widget.prompt({ layout = awful.widget.layout.horizontal.leftright })
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    mylayoutbox[s] = awful.widget.layoutbox(s)
    mylayoutbox[s]:buttons(awful.util.table.join(
                           awful.button({ }, 1, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 3, function () awful.layout.inc(layouts, -1) end),
                           awful.button({ }, 4, function () awful.layout.inc(layouts, 1) end),
                           awful.button({ }, 5, function () awful.layout.inc(layouts, -1) end)))
    -- Create a taglist widget
    mytaglist[s] = awful.widget.taglist(s, awful.widget.taglist.label.all, mytaglist.buttons)

    -- Create a tasklist widget
    mytasklist[s] = awful.widget.tasklist(function(c)
                                              return awful.widget.tasklist.label.currenttags(c, s)
                                          end, mytasklist.buttons)

    -- Create the wibox
    mywibox[s] = awful.wibox({ position = "top", height = "18", screen = s, ontop = nil })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            mylauncher,
            mytaglist[s],
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        mylayoutbox[s],
        mytextclock,
        mytextclock_icon,
        separator,
        s == 1 and mysystray or nil,
        s == 1 and separator or nil,
        volbar.widget,
        tb_batt,
        tb_net,
        separator,
        memwidget.widget,
        memicon,
        cpuwidget.widget,
        cpuicon,
        separator,
        tb_nv,
        icon_nv,
        sep_nv,
        tb_mails,
        separator,
        mytasklist[s],
        layout = awful.widget.layout.horizontal.rightleft
    }
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
    awful.key({ modkey,           }, "w", function () mymainmenu:show({keygrabber=true}) end),

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
              end)
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
   table.foreach(v, function(_, kk) table.insert(globalkeys, kk) end)
end
for k, v in pairs(persoclientkeys) do
   table.foreach(v, function(_, kk) table.insert(clientkeys, kk) end)
end
root.keys(globalkeys)
-- }}}

-- {{{ Rules
-- Special rules for special windows
function handle_graphite(c)
   local f = io.open("/proc/" .. c.pid .. "/comm")
   if f then
      local comm = f:read("*a")
      io.close(f)
      if comm == "graphite\n" then
         awful.client.floating.set(c, true)
      end
   end
end

-- Normal rules
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
                             "pinentry", "Plugin-container", "Smplayer", "VirtualBox", "Vlc",
                             "Wine", "Xfmedia", "xine", "XVroot" },
                   instance = { "pinentry-gtk-2" },
                   name = { "Gnuplot (window id : 0)", "Minecraft", "R Graphics: Device 2 (ACTIVE)" } },
      properties = { floating = true } },

    -- Complex rules
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
    { rule = { class = nil, instance = nil },
      callback = handle_graphite },
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
client.add_signal("manage", function (c, startup)
    -- Add a titlebar
    handle_titlebar(c)
    c:add_signal("property::floating", handle_titlebar)

    -- Enable sloppy focus
    c:add_signal("mouse::enter", function(c)
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
end)

client.add_signal("focus", function(c) c.border_color = beautiful.border_focus end)
client.add_signal("unfocus", function(c) c.border_color = beautiful.border_normal end)
-- }}}

-- {{{ Timers
mytimer5 = timer { timeout = 5 }
mytimer5:add_signal("timeout", function ()
    tb_batt.text = " " .. battery_mon() .. " "
    if tb_nv then tb_nv.text = nvtemp.format() end
end)
mytimer5:start()

mytimer15 = timer { timeout = 15 }
mytimer15:add_signal("timeout", function ()
    tb_net.text = " " .. netmon.netmon(nm_ifs, "8.8.8.8")
    tb_mails_update()
end)
mytimer15:start()

mytimer300 = timer { timeout = 300 }
mytimer300:add_signal("timeout", function ()
    awful.util.spawn(beautiful.wallpaper_cmd[1])
end)
mytimer300:start()
-- }}}

-- {{{ Disable startup-notification
-- http://awesome.naquadah.org/wiki/Disable_startup-notification_globally
local oldspawn = awful.util.spawn
awful.util.spawn = function (s)
  oldspawn(s, false)
end
-- }}}
