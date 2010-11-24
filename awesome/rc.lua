-- Standard awesome library
require("awful")
require("awful.autofocus")
require("awful.rules")
-- Theme handling library
require("beautiful")
-- Notification library
require("naughty")

-- Requis pour la date en français
os.setlocale("fr_FR.utf8")
-- ... mais les nombres doivent être en anglais (séparateur décimal...)
os.setlocale("C", "numeric")

-- {{{ Variable definitions

-- Themes define colours, icons, and wallpapers
beautiful.init("/home/schnouki/.config/awesome/themes/schnouki-zenburn.lua")

-- This is used later as the default terminal and editor to run.
terminal = "urxvt"
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
    awful.layout.suit.tile,
    awful.layout.suit.tile.bottom,
    awful.layout.suit.fair,
    awful.layout.suit.fair.horizontal,
    awful.layout.suit.max,
    awful.layout.suit.magnifier,
    awful.layout.suit.floating,
    awful.layout.suit.spiral,
    awful.layout.suit.spiral.dwindle,
}

-- }}}

-- {{{ Tags
-- Define a tag table which hold all screen tags.
tags = {}
for s = 1, screen.count() do
    -- Each screen has its own tag table.
    tags[s] = awful.tag({ 1, 2, 3, 4, 5, 6, 7, 8, 9 }, s, layouts[1])
end
-- }}}

-- {{{ Menu

-- Create a laucher widget and a main menu
myawesomemenu = {
   { "manual", terminal .. " -e man awesome" },
   { "edit config", editor_cmd .. " " .. awful.util.getdir("config") .. "/rc.lua" },
   { "restart", awesome.restart },
   { "quit", awesome.quit }
}

gamemenu = {
   { "frozen bubble", "frozen-bubble", image("/usr/share/pixmaps/frozen-bubble.png") },
   { "kobo deluxe", "kobodl", image("/usr/share/pixmaps/kobo-icon.xpm") },
   { "hedgewars", "hedgewars", image("/usr/share/pixmaps/hedgewars.png") },
}

utilsmenu = {
   { "galculator", "galculator", image("/usr/share/pixmaps/galculator.png") },
   { "gdmap", "gdmap", image("/usr/share/pixmaps/gdmap_icon.png") },
}

mymainmenu = awful.menu({ items = { { "awesome", myawesomemenu, beautiful.awesome_icon },
                                    { "jeux", gamemenu },
                                    { "utils", utilsmenu },
                                    { "gajim", "gajim", image("/usr/share/icons/hicolor/64x64/apps/gajim.png") },
                                    { "thunderbird", "thunderbird", image("/usr/share/pixmaps/thunderbird.png") },
                                    { "firefox", "firefox", image("/usr/share/pixmaps/firefox.png") },
                                    { "chromium", "chromium", image("/usr/share/icons/hicolor/16x16/apps/chromium.png") },
                                    { "picasa", "/usr/local/bin/picasa", image("/home/schnouki/.config/awesome/icons/picasa.png") },
                                    { "spotify", "spotify", image("/home/schnouki/.config/awesome/icons/spotify.png") },
                                    { "libre office", "soffice", image("/usr/share/icons/hicolor/16x16/apps/ooo-writer.png") },
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
mytextclock = awful.widget.textclock({ align = "right" }, " %a %e %b %Y %T %Z ", 1)

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
                                              if not c:isvisible() then
                                                  awful.tag.viewonly(c:tags()[1])
                                              end
                                              client.focus = c
                                              c:raise()
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
require("netmon")
netmon.init()
tb_net = widget({ type = "textbox" })
nm_ifs = { ["E"] = "eth0" }
if gethost() == "odin" then
   nm_ifs["W"] = "wlan0"
end
tb_net.text = " " .. netmon.netmon(nm_ifs, "8.8.8.8")

if gethost() == "thor" then
   require("nvtemp")
   nvtemp.init()
   tb_nv = widget({ type = "textbox" })
   tb_nv.text = " GPU: ... "
end

require("battmon")
tb_batt = widget({ type = "textbox" })
tb_batt.text = " " .. battery_mon() .. " "

require("volumebar")
pb_vol = widget({ type = "progressbar" }) -- TODO: migrer à awful.widget.progressbar...
pb_vol.width = 10
pb_vol.height = 1
pb_vol.border_padding = 0
pb_vol.ticks_count = 0
pb_vol.vertical = true
pb_vol:bar_properties_set("vol", {
       ["bg"]           = "#000000",
       ["fg_off"]       = "#000000",
       ["border_color"] = "#000000"
})
volume_upd(pb_vol, volume_get())

pb_vol:buttons(awful.util.table.join(
       awful.button({ }, 1, function () volume_upd(pb_vol, volume_plus() ) end),
       awful.button({ }, 4, function () volume_upd(pb_vol, volume_plus() ) end),
       awful.button({ }, 3, function () volume_upd(pb_vol, volume_minus()) end),
       awful.button({ }, 5, function () volume_upd(pb_vol, volume_minus()) end),
       awful.button({ }, 2, function () volume_upd(pb_vol, volume_mute() ) end)
))

-- Afficher des infos sur le client qui a le focus
-- d'après http://github.com/MajicOne/awesome-configs/blob/master/rc.lua
require('markup')
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

   tb_mails = widget({ type = "textbox" })
   tb_mails_color_normal   = "#7cb8bb" -- blue-1
   tb_mails_color_updating = "#ac7373" -- red-2
   tb_mails_color = tb_mails_color_normal
   function tb_mails_set_count(n)
      local s = markup.fg.color(tb_mails_color, " ✉ " .. n)
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

-- Spop status
require("spop")
spop.init("localhost", 6602)
tb_spop = widget({ type = "textbox" })
tb_spop.text = " [spop] "
-- }}}

-- {{{ Raccourcis claviers persos

persokeys = {
   -- Volume
   awful.key({ }, "XF86AudioRaiseVolume", function () volume_upd(pb_vol, volume_plus())  end),
   awful.key({ }, "XF86AudioLowerVolume", function () volume_upd(pb_vol, volume_minus()) end),
   awful.key({ }, "XF86AudioMute",        function () volume_upd(pb_vol, volume_mute())  end),

   awful.key({ modkey }, "Up",        function () volume_upd(pb_vol, volume_plus())  end),
   awful.key({ modkey }, "Down",      function () volume_upd(pb_vol, volume_minus()) end),
   awful.key({ modkey }, "KP_Delete", function () volume_upd(pb_vol, volume_mute())  end),

   -- F2 - verrouiller l'écran
   awful.key({ }, "XF86ScreenSaver",      function () os.execute("xscreensaver-command -lock") end),
   awful.key({ modkey, "Control" }, "F2", function () os.execute("xscreensaver-command -lock") end),
   -- F3 - éteindre l'écran
   awful.key({ }, "XF86Battery",          function () os.execute("xset dpms force suspend") end),
   awful.key({ modkey, "Control" }, "F3", function () os.execute("xset dpms force suspend") end),
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
   awful.key({ }, "XF86Launch1",      function () awful.util.spawn(editor_cmd) end),
   awful.key({ modkey }, "KP_Insert", function () awful.util.spawn(editor_cmd) end),

   -- Spop
   awful.key({ }, "XF86AudioStop", spop.stop),
   awful.key({ }, "XF86AudioPlay", spop.toggle),
   awful.key({ }, "XF86AudioPrev", spop.prev),
   awful.key({ }, "XF86AudioNext", spop.next),

   awful.key({ modkey, "Control" }, "Up",    spop.stop),
   awful.key({ modkey, "Control" }, "Down",  spop.toggle),
   awful.key({ modkey, "Control" }, "Left",  spop.prev),
   awful.key({ modkey, "Control" }, "Right", spop.next),

   awful.key({ modkey, "Mod1" }, "Down", spop.cycle),
}

persoclientkeys = {
   awful.key({ modkey, "Shift"   }, "t", function (c) if c.titlebar then awful.titlebar.remove(c) else awful.titlebar.add(c, { modkey = modkey }) end end),
   awful.key({ modkey            }, "s", function (c) c.sticky = not c.sticky end),
   awful.key({ modkey, "Shift"   }, "s", function (c) c.ontop = not c.ontop end),
   awful.key({ modkey            }, "i", win_info),
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
    mywibox[s] = awful.wibox({ position = "top", screen = s, ontop = nil })
    -- Add widgets to the wibox - order matters
    mywibox[s].widgets = {
        {
            mylauncher,
            mytaglist[s],
            mypromptbox[s],
            layout = awful.widget.layout.horizontal.leftright
        },
        mylayoutbox[s],
        s == 1 and mysystray or nil,
        mytextclock,
        pb_vol,
        tb_batt,
        tb_net,
        tb_nv,
        tb_mails,
        tb_spop,
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
    awful.key({ modkey,           }, "w", function () mymainmenu:show(true)        end),

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
    awful.key({ modkey,           }, "n",      function (c) c.minimized = not c.minimized    end),
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
   local comm = f:read("*a")
   io.close(f)
   if comm == "graphite\n" then
      awful.client.floating.set(c, true)
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
    { rule = { class = "MPlayer" },
      properties = { floating = true } },
    { rule = { class = "Smplayer" },
      properties = { floating = true } },
    { rule = { class = "xine" },
      properties = { floating = true } },
    { rule = { class = "pinentry" },
      properties = { floating = true } },
    { rule = { instance = "pinentry-gtk-2" },
      properties = { floating = true } },
    { rule = { class = "Gimp" },
      properties = { floating = true } },
    { rule = { class = "XVroot" },
      properties = { floating = true } },
    { rule = { class = "Galculator" },
      properties = { floating = true } },
    { rule = { class = "VirtualBox" },
      properties = { floating = true } },
    { rule = { class = "Gnote" },
      properties = { floating = true } },
    { rule = { class = "Wine" },
      properties = { floating = true } },
    { rule = { class = "Xfmedia" },
      properties = { floating = true } },
    { rule = { class = "Vlc" },
      properties = { floating = true } },
    { rule = { name = "Gnuplot (window id : 0)" },
      properties = { floating = true } },
    { rule = { name = "R Graphics: Device 2 (ACTIVE)" },
      properties = { floating = true } },
    { rule = { class = "Chromium" },
      properties = { tag = tags[1][3] } },
    { rule = { class = "Chromium", name = "Préférences de Chromium" },
      properties = { floating = true } },
    { rule = { class = "Firefox" },
      properties = { floating = true } },
    { rule = { class = "Firefox", instance = "Navigator" },
      properties = { tag = tags[1][3], floating = false } },
    { rule = { class = "Thunderbird" },
      properties = { floating = true } },
    { rule = { class = "Thunderbird", instance = "Mail" },
      properties = { tag = tags[1][2], floating = false } },
    { rule = { class = "Xchat" },
      properties = { tag = tags[1][1] } },
    { rule = { class = "Gajim.py", role = "roster" },
      properties = { tag = tags[1][1] } },
    { rule = { class = "Gajim.py", role = "messages" },
      callback = awful.client.setslave },
    { rule = { class = "Pino" },
      properties = { tag = tags[1][1] } },
    { rule = { instance = "spotify.exe" },
      properties = { tag = tags[1][9] } },
    { rule = { class = "Audacious" },
      properties = { floating = true, ontop = true, sticky = true } },
    { rule = { class = nil, instance = nil },
      callback = handle_graphite },
}
no_titlebar_apps = {
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
    volume_upd(pb_vol, volume_get())
    if tb_nv then tb_nv.text = " " .. nvtemp.format() .. " " end
end)
mytimer5:start()

mytimer15 = timer { timeout = 15 }
mytimer15:add_signal("timeout", function ()
    tb_net.text = " " .. netmon.netmon(nm_ifs, "8.8.8.8")
    tb_mails_update()
end)
mytimer15:start()
-- }}}