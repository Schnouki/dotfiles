#!/usr/bin/env lua

package.path = "../?.lua;" .. package.path

local io = require("io")
local parser = require("pulse_parser")
local inspect = require("inspect")
local __ = require("underscore")

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

local function pacmd(args)
   return get_cmd_output("pacmd " .. args)
end

local res = parser.parse_pacmd_list_cards(pacmd("list-cards"))
-- print(inspect.inspect(res))
print("Cards", inspect.inspect(__.chain(res)
                      :find(function(card) return card.index == 0 end)
                         -- :result("sinks")
                         -- :keys()
                         -- :first()
                         :value()))

local res = parser.parse_pacmd_list_sinks(pacmd("list-sources"))
print("Sources", inspect.inspect(res))
