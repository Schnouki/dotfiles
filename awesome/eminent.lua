----------------------------------------------------------------
-- Effortless wmii-style dynamic tagging.
----------------------------------------------------------------
-- Lucas de Vries <lucas@glacicle.org>
-- Licensed under the WTFPL version 2
--   * http://sam.zoy.org/wtfpl/COPYING
----------------------------------------------------------------
-- To use this module add:
--   require("eminent")
-- to the top of your rc.lua.
--
-- That's it. Through magical monkey-patching, all you need to
-- do to start dynamic tagging is loading it.
--
-- Use awesome like you normally would, you don't need to
-- change a thing.
----------------------------------------------------------------

-- Grab environment
local ipairs = ipairs
local pairs = pairs
local awful = require("awful")
local table = table
local capi = {
    tag = tag,
    mouse = mouse,
    client = client,
    screen = screen,
    wibox = wibox,
    timer = timer,
    keygrabber = keygrabber,
}

local getscreen = capi.tag.getscreen

-- Eminent: Effortless wmii-style dynamic tagging
local eminent = {}


-- Grab the original functions we're replacing
local deflayout = nil
local orig = {
    new = awful.tag.new,
    viewidx = awful.tag.viewidx,

    taglist = awful.widget.taglist.new,
    --label = awful.widget.taglist.label.all,
    label = awful.widget.taglist.filter.all,
}

-- Return tags with stuff on them, mark others hidden
function gettags(screen)
    local tags = {}

    --for k, t in ipairs(capi.screen[screen]:tags()) do
    for k, t in ipairs(awful.tag.gettags(screen)) do
        if t.selected or #t:clients() > 0 then
            awful.tag.setproperty(t, "hide", false)
            table.insert(tags, t)
        else
            awful.tag.setproperty(t, "hide", true)
        end
    end

    return tags
end

-- Pre-create tags
awful.tag.new = function (names, screen, layout)
    deflayout = layout and layout[1] or layout
    return orig.new(names, screen, layout)
end

-- Taglist label functions
--awful.widget.taglist.label.all = function (t, args)
awful.widget.taglist.filter.all = function (t, args)
    if t.selected or #t:clients() > 0 then
        return orig.label(t, args)
    end
end


-- Update hidden status
local function uc(c) gettags(c.screen) end
--local function ut(t) gettags(t.screen) end
local function ut(s,t) gettags(s.index) end


capi.client.connect_signal("unmanage", uc)
capi.client.connect_signal("new", function(c)
    c:connect_signal("property::screen", uc)
    c:connect_signal("property::urgent", uc)
    c:connect_signal("tagged", uc)
    c:connect_signal("untagged", uc)
    c:connect_signal("focus", uc)
    c:connect_signal("unfocus", uc)
end)

for screen=1, capi.screen.count() do
    awful.tag.attached_connect_signal(screen, "property::selected", ut)
    awful.tag.attached_connect_signal(screen, "property::icon", ut)
    awful.tag.attached_connect_signal(screen, "property::hide", ut)
    awful.tag.attached_connect_signal(screen, "property::name", ut)
    awful.tag.attached_connect_signal(screen, "property::activated", ut)
    awful.tag.attached_connect_signal(screen, "property::screen", ut)
    awful.tag.attached_connect_signal(screen, "property::index", ut)

    --awful.tag.attached_connect_signal(screen, "tag::history::update",uc)
    capi.screen[screen]:connect_signal("tag::history::update", ut)
    --capi.screen[screen]:connect_signal("tag::detach", ut)
end

return eminent
