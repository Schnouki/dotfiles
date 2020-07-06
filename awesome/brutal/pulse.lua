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
local __ = require("underscore")
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
local function get_sources()
    return parser.parse_pacmd_list_sinks(pacmd("list-sources"))
end
local function get_source_outputs()
    return parser.parse_pacmd_list_sinks(pacmd("list-source-outputs"))
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
        return dump.default_sink
    end
end
local function get_source_name(dump, source)
    if type(source) == "string" and dump.profile[source] then
        return source
    else
        return dump.default_source
    end
end

local function get_sink_inputs_by_role(role)
    local sink
    local sinks = {}
    local all_sinks = get_sink_inputs()
    for _, sink in ipairs(all_sinks) do
        if sink.prop["media.role"] == role then
            -- Parse volume
            local mtch = string.match(sink.attr.volume, "[%a-]+: (%d+)")
            if mtch ~= nil then
                sink.volume = tonumber(mtch) / 0x10000 * 100
            end
            sinks[sink.index] = sink
        end
    end
    return sinks
end

local function get_default_card()
    local dump = get_pacmd_dump()
    local sink_name = dump.default_sink

    local card = __.chain(get_cards())
        :find(function(c)
                return __.chain(c["sinks"])
                    :keys()
                    :contains(sink_name)
                    :value()
             end)
        :value()
    return card
end

local function profile_setter(card, profile_id, callback)
    return function()
        local card_name = string.match(card.attr.name, "<(.+)>")

        -- Find inputs currently using this card
        local card_inputs = {}
        local input
        for _, input in ipairs(get_sink_inputs()) do
            local sink = string.match(input.attr.sink, "<(.*)>")
            if sink and card["sinks"][sink] then
                table.insert(card_inputs, input.index)
            end
        end

        -- Set profile
        pacmd("set-card-profile " .. card_name .. " " .. profile_id)

        -- Get the new sink name
        local new_sink_name = __.chain(get_cards())
            :find(function(c) return c.index == card.index end)
            :result("sinks")
            :keys()
            :first()
            :value()

        -- Now move all inputs on this card to the correct sink
        for _, input in ipairs(card_inputs) do
            pacmd("move-sink-input " .. input .. " " .. new_sink_name)
        end

        callback()
    end
end
local function get_card_profiles_menu(card, callback)
    local profiles = {}
    local profiles_menu = {}
    local profile_id, profile
    for profile_id, profile in pairs(card.profile) do
        if profile.available ~= "no" then
            table.insert(profiles, {
                             id = profile_id,
                             name = profile.name,
                             prio = profile.priority,
                             active = card.active_profile == profile_id
            })
        end
    end
    table.sort(profiles, function(a, b) return a.prio > b.prio end)
    for _, profile in ipairs(profiles) do
        local prefix = "    "
        if profile.active then prefix = "✓ " end
        table.insert(profiles_menu, { prefix .. profile.name,
                                      profile_setter(card, profile.id, callback) })
    end
    profiles_menu.theme = { width = 400 }
    return profiles_menu
end
local function get_profiles_menu(callback)
    local cards = get_cards()
    local menu = {}
    local card
    for _, card in ipairs(cards) do
        local card_name = card.prop["device.description"]
        local card_icon = card.prop["device.icon_name"]
        local profiles_menu = get_card_profiles_menu(card, callback)
        table.insert(menu, { card_name, profiles_menu, get_icon(card_icon) })
    end
    return menu
end

local function sink_setter(sink, callback)
    return function()
        pacmd("set-default-sink " .. sink)

        local inputs = get_sink_inputs()
        local input
        for _, input in ipairs(inputs) do
            if not string.match(input.attr.sink, "^" .. sink .. " <.*>") then
                pacmd("move-sink-input " .. input.index .. " " .. sink)
            end
        end

        callback()
    end
end
local function source_setter(source, callback)
    return function()
        pacmd("set-default-source " .. sink)

        local outputs = get_source_outputs()
        local output
        for _, output in ipairs(outputs) do
            if not string.match(output.attr.source, "^" .. source .. " <.*>") then
                pacmd("move-source-output " .. output.index .. " " .. source)
            end
        end

        callback()
    end
end

local function get_sinks_sources_menu(all_sinks, default_sink, setter, callback)
    local prefix, sink
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
                             setter(sink.index, callback),
                             get_icon(sink.icon) })
    end
    menu.theme = { width = 250 }
    return menu
end
local function get_sinks_menu(callback)
    local all_sinks = get_sinks()
    local default_sink = get_pacmd_dump().default_sink
    local setter = sink_setter

    return get_sinks_sources_menu(all_sinks, default_sink, setter, callback)
end
local function get_sources_menu(callback)
    local all_sources = get_sources()
    local default_source = get_pacmd_dump().default_source
    local setter = source_setter

    return get_sinks_sources_menu(all_sources, default_source, setter, callback)
end

local function sink_port_setter(sink, port, callback)
    return function()
        pacmd("set-sink-port " .. sink .. " " .. port)
        callback()
    end
end
local function source_port_setter(source, port, callback)
    return function()
        pacmd("set-source-port " .. source .. " " .. port)
        callback()
    end
end

local function get_ports_menu(all_sinks, setter, callback)
    local prefix, sink
    local menu = {}
    local sinks = {}

    for _, sink in ipairs(all_sinks) do
        if sink.ports ~= nil then
            local ports = {}
            local ports_menu = {}
            local port, port_id
            for port_id, port in pairs(sink.ports) do
                if port.available ~= "no" then
                    table.insert(ports, {
                                     id = port_id,
                                     name = port.name,
                                     prio = port.priority,
                                     active = sink.active_port == port_id
                    })
                end
            end
            table.sort(ports, function(a, b) return a.prio > b.prio end)

            for _, port in ipairs(ports) do
                local prefix = "    "
                if port.active then prefix = "✓ " end
                table.insert(ports_menu, { prefix .. port.name,
                                           setter(sink.index, port.id, callback) })
            end
            ports_menu.theme = { width = 400 }
            table.insert(menu, { sink.prop["device.description"],
                                 ports_menu })
        end
    end
    menu.theme = { width = 250 }
    return menu
end
local function get_sink_ports_menu(callback)
    local all_sinks = get_sinks()
    return get_ports_menu(all_sinks, sink_port_setter, callback)
end
local function get_source_ports_menu(callback)
    local all_sources = get_sources()
    return get_ports_menu(all_sources, source_port_setter, callback)
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

function pulse.get_default_source()
    local data = get_pacmd_dump()
    local source_name = get_source_name(data, nil)
    if source_name == nil then return end
    local all_sources = get_sources()

    for _, source in ipairs(all_sources) do
        local name = string.match(source.attr.name, "<(.+)>")
        if name == source_name then
            local mtch = string.match(source.attr.volume, "[%a-]+: (%d+)")
            if mtch ~= nil then
                local volume = tonumber(mtch) / 0x10000 * 100
                return { source.prop["device.description"], volume }
            end
        end
    end
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
    local default_card = get_default_card()
    local menu_items = {
        { "Default sink",  get_sinks_menu(callback) },
        { "Default source",  get_sources_menu(callback) },
        { "Card profiles", get_card_profiles_menu(default_card, callback) },
        { "Sink port", get_sink_ports_menu(callback) },
        { "Source port", get_source_ports_menu(callback) },
        { "All profiles",  get_profiles_menu(callback) }
    }
    return awful.menu({ items = menu_items })
end
-- }}}

return setmetatable(pulse, { __call = function(_, ...) return worker(...) end })

-- Local Variables:
-- lua-indent-level: 4
-- End:
