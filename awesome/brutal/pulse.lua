---------------------------------------------------
-- Licensed under the GNU General Public License v2
--  * (c) 2010, MrMagne <mr.magne@yahoo.fr>
--  * (c) 2010, Mic92 <jthalheim@gmail.com>
--  * (c) 2014, Thomas Jost <schnouki@schnouki.net>
---------------------------------------------------

-- {{{ Grab environment
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
    find = string.find,
    match = string.match,
    format = string.format,
    gmatch = string.gmatch
}
local math = {
    floor = math.floor,
    tointeger = math.tointeger
}
local awful = require("awful")
-- }}}


-- Pulse: provides volume levels of requested pulseaudio sinks and methods to change them
-- vicious.contrib.pulse
local pulse = {}

-- {{{ Helper function
local function pacmd(args)
    local f = io.popen("pacmd "..args)
    if f == nil then
        return nil
    else
        local line = f:read("*all")
        f:close()
        return line
    end
end
local function pacmd_lines(args)
    local f = io.popen("pacmd " .. args)
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

local function escape(text)
    local special_chars = { ["."] = "%.", ["-"] = "%-" }
    return text:gsub("[%.%-]", special_chars)
end

local cached_sinks = {}
local function get_sink_name(sink)
    if type(sink) == "string" then return sink end
    -- avoid nil keys
    local key = sink or 1
    -- Cache requests
    if not cached_sinks[key] then
        local line, name, prio, mtch
        local raw_sinks = {}
        for line in pacmd_lines("list-sinks") do
            mtch = string.match(line, "name: <(.-)>")
            if mtch ~= nil then
                name = mtch
            else
                mtch = string.match(line, "priority: (%d+)")
                if mtch then
                    prio = tonumber(mtch)
                    table.insert(raw_sinks, {
                                     name = name,
                                     prio = prio,
                    })
                end
            end
        end

        -- Sort sinks by priority
        table.sort(raw_sinks, function(a, b) return a.prio > b.prio end)

        -- Move to cached_sinks
        cached_sinks = {}
        local idx, entry
        for idx, entry in ipairs(raw_sinks) do
            table.insert(cached_sinks, entry.name)
        end
    end

    return cached_sinks[key]
end

local function get_profiles()
    local profiles = {}
    local active_profile = nil
    local in_profiles = false
    local line, mtch, profile
    local name, desc, prio
    for line in pacmd_lines("list-cards") do
        -- Try to find a section header
        mtch = string.match(line, "^\t(%w+[%w%s]*):")
        if mtch ~= nil then
            local section = mtch
            if section == "profiles" then
                in_profiles = true
            else
                in_profiles = false
                if section == "active profile" then
                    mtch = string.match(line, ".*: <(.*)>")
                    active_profile = mtch
                end
            end
        elseif in_profiles then
            for name, desc, prio in string.gmatch(line, "%s*(output:.*): (.+) %(priority (%d+)") do
                table.insert(profiles, {
                    name = name,
                    desc = desc,
                    prio = tonumber(prio),
                })
            end
        end
    end

    table.sort(profiles, function(a, b) return a.prio > b.prio or (a.prio == b.prio and a.desc < b.desc) end)
    for _, profile in ipairs(profiles) do
        profile.active = profile.name == active_profile
    end
    return profiles
end

local cached_profiles_menu = nil
local function set_profile(name)
    cached_sinks = {}
    cached_profiles_menu = nil
    pacmd("set-card-profile 0 " .. name)
end

local function get_profiles_menu(update_func, names)
    if cached_profiles_menu ~= nil then
        return cached_profiles_menu
    end

    local all_profiles = get_profiles()
    local profiles = {}
    local menu_items = {}
    local desc, profile
    if names == nil then
        profiles = all_profiles
    else
        for _, name in ipairs(names) do
            for _, profile in ipairs(all_profiles) do
                if profile.name == name then
                    table.insert(profiles, profile)
                    break
                end
            end
        end
    end

    -- Display the menu
    for _, profile in ipairs(profiles) do
        if profile.active then desc = "✓ " else desc = "    " end
        table.insert(menu_items, { desc .. profile.desc,
                                   function()
                                       set_profile(profile.name)
                                       update_func()
        end })
    end
    cached_profiles_menu = awful.menu({ items = menu_items,
                                        theme = { width = 350 }})
    return cached_profiles_menu
end
-- }}}

-- {{{ Pulseaudio widget type
local function worker(format, sink)
    sink = get_sink_name(sink)
    if sink == nil then return {0, "unknown"} end

    -- Get sink data
    local data = pacmd("dump")

    -- If mute return 0 (not "Mute") so we don't break progressbars
    if string.find(data,"set%-sink%-mute "..escape(sink).." yes") then
        return {0, "off"}
    end

    local vol = tonumber(string.match(data, "set%-sink%-volume "..escape(sink).." (0x[%x]+)"))
    if vol == nil then vol = 0 end

    return { math.floor(vol/0x10000*100), "on"}
end
-- }}}

-- {{{ Volume control helper
function pulse.add(percent, sink)
    sink = get_sink_name(sink)
    if sink == nil then return end

    local data = pacmd("dump")

    local pattern = "set%-sink%-volume "..escape(sink).." (0x[%x]+)"
    local initial_vol =  tonumber(string.match(data, pattern))

    local vol = math.tointeger(math.floor(initial_vol + percent/100*0x10000))
    if vol > 0x10000 then vol = 0x10000 end
    if vol < 0 then vol = 0 end

    local cmd = string.format("pacmd set-sink-volume %s 0x%x >/dev/null", sink, vol)
    return os.execute(cmd)
end

function pulse.toggle(sink)
    sink = get_sink_name(sink)
    if sink == nil then return end

    local data = pacmd("dump")
    local pattern = "set%-sink%-mute "..escape(sink).." (%a%a%a?)"
    local mute = string.match(data, pattern)

    -- 0 to enable a sink or 1 to mute it.
    local state = { yes = 0, no = 1}
    local cmd = string.format("pacmd set-sink-mute %s %d", sink, state[mute])
    return os.execute(cmd)
end
-- }}}

-- {{{ Card profiles helpers
function pulse.profiles_menu(names)
    return get_profiles_menu(names)
end
-- }}}

return setmetatable(pulse, { __call = function(_, ...) return worker(...) end })

-- Local Variables:
-- lua-indent-level: 4
-- End:
