local lpeg = require("lpeg")

local C, Cc, Cf, Cg, Ct, P, R, S = lpeg.C, lpeg.Cc, lpeg.Cf, lpeg.Cg, lpeg.Ct, lpeg.P, lpeg.R, lpeg.S

local function concat(a, b)
   return a .. b
end
local function split(s, sep)
   local psep = P(sep)
   local elem = C((1 - psep)^0)
   local p = Ct(elem * (psep * elem)^0)
   return p:match(s)
end

local SP = S" "
local TAB = S"\t"
local LF = P"\n"
local REST = (1 - LF)^0
local LINE = REST * LF
local INT = R"09"^1 / tonumber

local identifier = C((1 - S":")^0) * S":"

local sink_index = (P"    " + P"  * ") * P"index: " * INT * LF
local attr_ident = TAB * identifier * SP^1
local attr_value = Cf(C(REST) * (C(LF) * TAB * SP^1 * C(REST))^0 * LF, concat)
local attr = Cg(attr_ident * attr_value)
local attrs = Cf(Ct("") * attr^1, rawset)

local prop_header = TAB * "properties:" * LF
local prop_identifier = C((1 - S"= ")^0) * SP^0 * S"="
local prop_value = '"' * C((1 - S'"')^0) * '"'
local prop_line = Cg(TAB * TAB * prop_identifier * SP^0 * prop_value * LF)
local prop_lines = Cf(Ct("") * prop_line^1, rawset)

local ports = TAB * "ports:" * LF * (TAB * TAB * LINE)^1 * (TAB * "active port: " * LINE)^-1
local sinks = TAB * "sinks:" * LF * (TAB * TAB * LINE)^1
local sources = TAB * "sources:" * LF * (TAB * TAB * LINE)^1

local profile_header = TAB * "profiles:" * LF
local profile_ident = C((1 - P": ")^1) * ":"
local profile_name = C((1 - P" (priority")^1)
local profile_prio = " (priority " * INT
local profile_avail = ", available: " * C((1 - P")")^1)
local profile_value = Cg(profile_name, "name") * Cg(profile_prio, "priority") * Cg(profile_avail, "available") * REST
local profile_line = Cg(TAB * TAB * profile_ident * SP^1 * Ct(profile_value) * LF)
local profile_lines = Cf(Ct("") * profile_line^1, rawset)
local profile_active = TAB * "active profile: <" * C((1 - S">")^1) * ">" * LF

local port_header = TAB * "ports:" * LF
local port_ident = C((1 - P": ")^1) * ":"
local port_name = C((1 - P" (priority")^1)
local port_prio = " (priority " * INT
local port_avail = (1 - P", available")^0 * ", available: " * C((1 - P")")^1)
local port_value = Cg(port_name, "name") * Cg(port_prio, "priority") * Cg(port_avail, "available") * REST
local port_props = TAB * TAB * TAB * LINE
local port_line = Cg(TAB * TAB * port_ident * SP^1 * Ct(port_value) * LF) * port_props^0
local port_lines = Cf(Ct("") * port_line^1, rawset)
local port_active = TAB * "active port: <" * C((1 - S">")^1) * ">" * LF

local card_sink_header = TAB * "sinks:" * LF
local card_source_header = TAB * "sources:" * LF
local card_sink_name = C((1 - P"/")^1) * "/"
local card_sink_index = "#" * C((1 - P": ")^1) * ":"
local card_sink_value = Cg(card_sink_index, "index") * SP^1 * Cg(REST, "desc")
local card_sink_line = Cg(TAB * TAB * card_sink_name * Ct(card_sink_value) * LF)
local card_sink_lines = Cf(Ct("") * card_sink_line^1, rawset)

local sink = Ct(Cg(sink_index, "index") *
                   Cg(attrs, "attr") *
                   prop_header *
                   Cg(prop_lines, "prop") *
                   (port_header *
                       Cg(port_lines, "ports") *
                       Cg(port_active, "active_port"))^-1)

local list_sinks_parser = LINE * Ct(sink^0)


local card = Ct(Cg(sink_index, "index") *
                   Cg(attrs, "attr") *
                   prop_header *
                   Cg(prop_lines, "prop") *
                   profile_header *
                   Cg(profile_lines, "profile") *
                   Cg(profile_active, "active_profile") *
                   card_sink_header *
                   Cg(card_sink_lines, "sinks") *
                   card_source_header *
                   Cg(card_sink_lines, "sources") *
                   ports^-1)

local list_cards_parser = LINE * Ct(card^0)

local info_line = identifier * SP^1 * C(REST) * LF
local info_parser = Cf(Ct("") * Cg(info_line)^1, rawset)

function parse_pacmd_list_sinks(data)
   return list_sinks_parser:match(data)
end

function parse_pacmd_list_cards(data)
   return list_cards_parser:match(data)
end

function parse_pactl_info(data)
   return info_parser:match(data)
end

function parse_pacmd_dump(lines)
   local line
   local ret = {
      ["default"] = nil,
      ["mute"] = {},
      ["profile"] = {},
      ["volume"] = {}
   }
   local funcs = {
      ["set-card-profile"] = function(card, profile)
         ret["profile"][card] = profile
      end,
      ["set-default-sink"] = function(sink)
         ret["default_sink"] = sink
      end,
      ["set-default-source"] = function(source)
         ret["default_source"] = source
      end,
      ["set-sink-mute"] = function(sink, val)
         ret["mute"][sink] = val == "yes"
      end,
      ["set-source-mute"] = function(source, val)
         ret["mute"][source] = val == "yes"
      end,
      ["set-sink-volume"] = function(sink, volume)
         ret["volume"][sink] = tonumber(volume) / 0x10000*100
      end,
      ["set-source-volume"] = function(source, volume)
         ret["volume"][source] = tonumber(volume) / 0x10000*100
      end
   }
   for line in lines do
      if string.len(line) > 0 then
         local tokens = split(line, " ")
         local func = funcs[tokens[1]]
         if func ~= nil then
            func(table.unpack(tokens, 2))
         end
      end
   end
   return ret
end

local parser = {
   parse_pacmd_list_sinks = parse_pacmd_list_sinks,
   parse_pacmd_list_cards = parse_pacmd_list_cards,
   parse_pactl_info = parse_pactl_info,
   parse_pacmd_dump = parse_pacmd_dump
}

return parser
