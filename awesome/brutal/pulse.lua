---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2010, MrMagne <mr.magne@yahoo.fr>
--  * (c) 2010, Mic92 <jthalheim@gmail.com>
--  * (c) 2016, Thomas Jost <schnouki@schnouki.net>
---------------------------------------------------

-- {{{ Grab environment
local pairs = pairs
local ipairs = ipairs
local type = type
local tonumber = tonumber
local io = { popen = io.popen }
local setmetatable = setmetatable
local os = { execute = os.execute }
local table = {
    insert = table.insert,
    sort = table.sort
}
local string = {
    match = string.match,
    format = string.format
}
local math = {
    floor = math.floor,
    tointeger = math.tointeger
}

local awful = require("awful")
local icon_theme = require("icon_theme")

local parser = require("brutal/pulse_parser")
-- }}}


-- Pulse: provides volume levels of requested pulseaudio sinks and methods to change them
-- vicious.contrib.pulse
local pulse = {}

-- {{{ Helper function
local function get_cmd_output(cmd)
    local f = io.popen(cmd)
    if f == nil then
        return nil
    else
        local line = f:read("*all")
        f:close()
        return line
    end
end
local function get_cmd_lines(cmd)
    local f = io.popen(cmd)
    if f == nil then
        return nil
    else
        local lines_it = f:lines()
        return function ()
            local line = lines_it()
            if line == nil then
                f:close()
            end
            return line
        end
    end
end

local function pacmd(args)       return get_cmd_output("pacmd " .. args) end
local function pacmd_lines(args) return get_cmd_lines("pacmd " .. args)  end

local function get_pacmd_dump()
    return parser.parse_pacmd_dump(pacmd_lines("dump"))
end
local function get_sinks()
    return parser.parse_pacmd_list_sinks(pacmd("list-sinks"))
end
local function get_sink_inputs()
    return parser.parse_pacmd_list_sinks(pacmd("list-sink-inputs"))
end
local function get_cards()
    return parser.parse_pacmd_list_cards(pacmd("list-cards"))
end

local function get_icon(name)
    local icon = icon_theme.get("devices", name)
    if icon == nil then
        local default = string.match(name, "(%w+-%w+)-%w+")
        icon = icon_theme.get("devices", default)
    end
    return icon
end

local function get_sink_name(dump, sink)
    if type(sink) == "string" and dump.profile[sink] then
        return sink
    else
        return dump.default
    end
end

local function get_sink_inputs_by_role(role)
    local sink
    local sinks = {}
    local all_sinks = get_sink_inputs()
    for _, sink in ipairs(all_sinks) do
        if sink.prop["media.role"] == role then
            -- Parse volume
            mtch = string.match(sink.attr.volume, "[%a-]+: (%d+)")
            if mtch ~= nil then
                sink.volume = tonumber(mtch) / 0x10000 * 100
            end
            sinks[sink.index] = sink
        end
    end
    return sinks
end

local function profile_setter(card_id, profile_id, callback)
    return function()
        pacmd("set-card-profile " .. card_id .. " " .. profile_id)
        callback()
    end
end
local function get_profiles_menu(callback)
    local card, prefix, profile_id, profile
    local cards = get_cards()
    local menu = { }
    for _, card in ipairs(cards) do
        local card_name = card.prop["device.description"]
        local card_icon = card.prop["device.icon_name"]
        local profiles = {}
        local profiles_menu = {}
        for profile_id, profile in pairs(card.profile) do
            table.insert(profiles, {
                             id = profile_id,
                             name = profile.name,
                             prio = profile.priority,
                             active = card.active_profile == profile_id
            })
        end
        table.sort(profiles, function(a, b) return a.prio > b.prio end)
        for _, profile in ipairs(profiles) do
            if profile.active then prefix = "✓ " else prefix = "   " end
            table.insert(profiles_menu, { prefix .. profile.name,
                                          profile_setter(card.index, profile.id, callback) })
        end
        profiles_menu.theme = { width = 400 }
        table.insert(menu, { card_name, profiles_menu, get_icon(card_icon) })
    end
    return menu
end

local function sink_setter(sink, callback)
    return function()
        pacmd("set-default-sink " .. sink)
        callback()
    end
end
local function get_sinks_menu(callback)
    local prefix, sink
    local all_sinks = get_sinks()
    local dump = get_pacmd_dump()
    local default_sink = dump.default
    local menu = {}
    local sinks = {}

    for _, sink in ipairs(all_sinks) do
        table.insert(sinks, {
                         index = sink.index,
                         name = sink.prop["device.description"],
                         icon = sink.prop["device.icon_name"],
                         prio = tonumber(sink.attr.priority),
                         default = sink.attr.name == "<" .. default_sink .. ">"
        })
    end
    table.sort(sinks, function(a, b) return a.prio > b.prio end)
    for _, sink in ipairs(sinks) do
        local name = sink.name
        if sink.default then prefix = "✓ " else prefix = "   " end
        table.insert(menu, { prefix .. sink.name,
                             sink_setter(sink.index, callback),
                             get_icon(sink.icon) })
    end
    menu.theme = { width = 250 }
    return menu
end
-- }}}

-- {{{ Pulseaudio widget type
local function worker(format, sink)
    -- Get data
    local data = get_pacmd_dump()

    -- Fix sink name if needed
    sink = get_sink_name(data, sink)
    if sink == nil then return {0, "unknown"} end

    -- If mute return 0 (not "Mute") so we don't break progressbars
    if data.mute[sink] then
        return {0, "off"}
    end

    return {data.volume[sink], "on"}
end
-- }}}

-- {{{ Volume control helper
local function get_new_volume(initial_vol, percent)
    local new_vol = math.floor(initial_vol + percent + .5)
    local vol = math.tointeger(math.floor(new_vol / 100 * 0x10000))
    if vol > 0x10000 then vol = 0x10000 end
    if vol < 0 then vol = 0 end
    return vol
end

function pulse.add(percent, sink)
    local data = get_pacmd_dump()
    sink = get_sink_name(data, sink)
    if sink == nil then return end

    local initial_vol = data.volume[sink]
    local new_vol = get_new_volume(initial_vol, percent)
    local cmd = string.format("pacmd set-sink-volume %s 0x%x >/dev/null", sink, new_vol)
    return os.execute(cmd)
end

function pulse.add_role(percent, role)
    local sinks = get_sink_inputs_by_role(role)
    local sink_idx, sink_data
    for sink_idx, sink_data in pairs(sinks) do
        local vol = get_new_volume(sink_data.volume, percent)
        local cmd = string.format("pacmd set-sink-input-volume %s 0x%x >/dev/null", sink_idx, vol)
        os.execute(cmd)
    end
end

function pulse.get_role(role)
    return get_sink_inputs_by_role(role)
end

function pulse.toggle(sink)
    local data = get_pacmd_dump()
    sink = get_sink_name(data, sink)
    if sink == nil then return end
    local mute = data.mute[sink]

    -- 0 to enable a sink or 1 to mute it.
    local state = { [true] = 0, [false] = 1}
    local cmd = string.format("pacmd set-sink-mute %s %d", sink, state[mute])
    return os.execute(cmd)
end
-- }}}

-- {{{ Menu builder
function pulse.menu(callback)
    local menu_items = {
        { "Profiles",     get_profiles_menu(callback) },
        { "Default sink", get_sinks_menu(callback) }
    }
    return awful.menu({ items = menu_items })
end
-- }}}

return setmetatable(pulse, { __call = function(_, ...) return worker(...) end })

-- Local Variables:
-- lua-indent-level: 4
-- End:
