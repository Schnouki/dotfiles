-- Document key bindings
--
-- Based on keydoc from
--	http://awesome.naquadah.org/wiki/Document_keybindings
--
-- with some patches and additions:
-- 	changed obsolete module declaration
--	public settings:
--		translation table 
--		colors and font
--	several notifications if not fit on screen
--

local awful = require("awful")
local table = table
local ipairs = ipairs
local pairs = pairs
local math = math
local string = string
local type = type
local modkey = modkey or "Mod4"
local beautiful = require("beautiful")
local naughty = require("naughty")
local capi = {
	root = root,
	client = client
}

-- module and public settings
local keydoc = {}
--
-- translation table
keydoc.translate = {}
-- colors and font
keydoc.color = {
	fg_key = beautiful.fg_end_widget,
	fg_doc = beautiful.fg_widget,
	fg_group = beautiful.fg_widget,
}
keydoc.font = beautiful.monofont

local doc = { }
local currentgroup = "Misc"
local orig = awful.key.new

-- Replacement for awful.key.new
local function new(mod, key, press, release, docstring)
	-- Usually, there is no use of release, let's just use it for doc
	-- if it's a string.
	if press and release and not docstring
	and type(release) == "string" then
		docstring = release
		release = nil
	end
	local k = orig(mod, key, press, release)
	-- Remember documentation for this key (we take the first one)
	if k and #k > 0 and docstring then
		doc[k[1]] = { help = docstring, group = currentgroup }
	end
	return k
end
awful.key.new = new	-- monkey patch

-- Turn a key to a string
local function key2str(key)
	local sym = key.key or key.keysym
	local translate = { -- translation defaults
		[modkey] = "⊞",
		Shift    = "⇧",
		Control  = "Ctrl",
		["#14"] = "#",
		[" "] = "Space",
	}
	if keydoc.translate then
		-- join public translation table
		translate = awful.util.table.join(
			translate,keydoc.translate)
	end
	sym = translate[sym] or sym
	if not key.modifiers or #key.modifiers == 0 then return sym end
	local result = ""
	for _, mod in pairs(key.modifiers) do
		mod = translate[mod] or mod
		result = result .. mod .. " + "
	end
	return result .. sym
end

-- Unicode "aware" length function (well, UTF8 aware)
-- See: http://lua-users.org/wiki/LuaUnicode
local function unilen(str)
	local _, count = string.gsub(str, "[^\128-\193]", "")
	return count
end

-- Start a new group
function keydoc.group(name)
	currentgroup = name
	return {}
end

-- match key against the given pattern
local function match_key(key,pattern)
	-- no pattern given
	if not pattern then return true end

	pattern = string.lower(pattern)

	-- eval key when single char given
	if string.len(pattern) == 1 then
		if key.key == pattern or key.keysym == pattern
		then return true
		else return false end
	-- eval help and key when string given
	elseif string.find(string.lower(doc[key].help),pattern)
	or string.find(string.lower(key.key),pattern)
	or string.find(string.lower(key.keysym),pattern)
	then return true
	else return false end
end

local fg_key, fg_doc, fg_group -- markup colors
local function markup(keys,pattern)
	local result = {}

	-- set markup colors
	fg_key = keydoc.color.fg_key or beautiful.fg_end_widget
	fg_doc = keydoc.color.fg_doc or beautiful.fg_widget
	fg_group = keydoc.color.fg_group or beautiful.fg_widget

	-- Compute longest key combination
	local longest = 0
	for _, key in ipairs(keys) do
		if doc[key] then
			longest = math.max(longest, unilen(key2str(key)))
		end
	end

	local curgroup = nil
	for _, key in ipairs(keys) do
		if doc[key] then
			local help, group = doc[key].help, doc[key].group
			local skey = key2str(key)
			-- match key against pattern
			if match_key(key,pattern) then
			result[group] = (result[group] or "") ..
				'<span font="'..keydoc.font..
				'" color="'..fg_key..'"> '..
				string.format(
					"%"..(longest - unilen(skey))..
					"s%s%2s", "",skey,""
				)..
				' </span><span color="'..fg_doc..'">'.. 
				help .. '</span>\n'
			end
		end
	end

   return result
end

-- Customize version of standard function pairs that sort keys
-- (from Michal Kottman on Stackoverflow)
local function spairs(t, order)
	-- collect the keys
	local keys = {}
	for k in pairs(t) do keys[#keys+1] = k end

	-- if order function given, sort by it by
	-- passing the table and keys a, b,
	-- otherwise just sort the keys 
	if order then
		table.sort(keys, function(a,b) return order(t, a, b) end)
	else
		table.sort(keys)
	end

	-- return the iterator function
	local i = 0
	return function()
		i = i + 1
		if keys[i] then
			return keys[i], t[keys[i]]
		end
	end
end

-- check notification height
local function check_size(txt,height)
	local tbox = wibox.widget.textbox()
	tbox:set_text(txt)
	local w,h = tbox:fit(-1,-1)
	tbox = false
	if h > height then return false
	else return true end
end

-- show notification
-- 	args: timeout, bg
-- 	control keys:
-- 		Esc - dismiss
-- 		space - next notification (if exists)
-- 		/ - run keydoc.search
--
local notif = {}
local function show(txt,run,args)
	args = args or {} -- naughty args

	-- check message body
	if not txt then
		if notif then naughty.destroy(notif) end
		return false
	end
	-- assign timeout for naughty and keygrabber
	local timeout = args.timeout or 30

	--{ control notification by keys
	local keytimer = false
	-- stop grabbing keys on timer (timeout = naughty timeout)
	if timeout ~= 0 then
		keytimer = timer({ timeout = timeout })
		keytimer:connect_signal("timeout", function()
			keytimer:stop()
			keygrabber.stop()
		end)
		keytimer:start()
	end
	-- catch some keys
	local catch = function(mod,key,event)
		if event == "release" then return end
		keygrabber.stop()
		if keytimer then keytimer:stop() end
		if key == "/" then
			-- help search
			naughty.destroy(notif)
			keydoc.search(args)
		elseif key == " " then
			-- paging
			if run then run()
			else naughty.destroy(notif) end
		elseif key == "Escape" then
			-- dismiss help
			naughty.destroy(notif)
		else
			return false
		end
	end
	keygrabber.run(catch)
	--}

	-- show notification
	notif = naughty.notify({
		text = txt,
		replaces_id = notif.id,
		run = function()
			-- stop grabbing keys
			keygrabber.stop()
			if keytimer then keytimer:stop() end
			-- perform run action
			if run then run()
			else naughty.destroy(notif) end
		end,
		timeout = timeout,
		bg = args.bg or beautiful.bg_focus,
	})
end

-- Display help in a naughty notification with respect to workarea
-- height. If the help text does not fit, it will be splitted to
-- several notifications shown one by one on mouse click.
--
local function compose(pattern,args)
	args = args or {} -- naughty args

	-- help strings
	local strings = markup(awful.util.table.join(
		capi.root.keys(),
		capi.client.focus and capi.client.focus:keys() or {}
		),
		pattern
	)

	-- calculate maximum height
	local preset = awful.util.table.join(naughty.config.defaults or {},
		naughty.config.presets.normal or {})
	local maxheight = screen[mouse.screen].workarea.height -
		(preset.border_width or 0) * 2 -
		(naughty.config.padding or 0) * 2

	-- compose text blocks fit to maximum height
	local result, ii = { "" }, 1
	if pattern then
		result[ii] = '<span weight="bold" color="'..fg_key..'">'..
			"Keys matching '"..pattern.."'".."</span>\n"
	end
	local txt = nil
	for group, res in spairs(strings) do
		empty = false
		if #result[ii] > 0 then result[ii] = result[ii] .. "\n" end
		txt = '<span weight="bold" color="'..fg_group..'">'..
			group .. "</span>\n" .. res
		if check_size(result[ii]..txt,maxheight) then
			result[ii] = result[ii] .. txt
		else -- next text block
			result[ii] = result[ii] ..
				'<span color="'.. fg_key..'">'..
				"\t\t--- click for more ---" .. "</span>"
			ii = ii + 1
			result[ii] = txt
		end
	end
	if not txt then -- no keys in strings
		result[ii] = result[ii]..'<span color="'..fg_key..'">'..
			"Not found".."</span>"
	end

	-- construct notification calls for every text block
	local call = {}
	ii = #result
	while ii > 0 do
		local txt, run = result[ii], false
		if call[ii+1] then run = call[ii+1] end
		call[ii] = function() show(txt,run,args) end
		ii = ii -1
	end
	-- execute notification chain
	call[1]()
end

-- Display the complete help message
-- notification parameters accepted by args:
-- 	timeout - notification timeout
-- 	bg - notification background color
-- other parameters can be changed through naughty config and preset
--
function keydoc.display(args) compose(false,args) end

-- Display help for keys corresponding with the pattern entered
function keydoc.search(args)
	awful.prompt.run({prompt = " Search key help: "},
		mypromptbox[mouse.screen].widget,
		function(str) compose(str,args) end
	)
end

return keydoc
