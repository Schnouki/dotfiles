function exists(filename)
	local file = io.open(filename)
	if file then
		io.close(file)
		return true
	else
		return false
	end
end

-- Texte en couleur
function col(color, text)
	return "<span color=\"" .. color .. "\">" .. text .. "</span>"
end

-- État des batteries
function battery_mon()
	local cur_cap = 0
	local full_cap = 0
	local rate = 0
	local state = 0 -- 0: idle, 1: charge, -1: discharge
	local time = 0
	local status = ""
	local i = 0

	while true do
		-- Est-ce que la batterie n° i existe ?
		local batname = "/proc/acpi/battery/BAT" .. i
		i = i+1
		if exists(batname) then
			-- Oui, on en extrait toutes les infos qu'il nous faut
			local bat = io.open(batname .. "/state")
			local text = bat:read("*a")
			io.close(bat)

			-- Est-ce que la batterie est là ?
			local present = string.match(text, "present: %s*(%w+)")
			if present ~= "no" then
				local bat_rate = string.match(text, "present rate:%s*(%d+)")
				local bat_cap = string.match(text, "remaining capacity:%s*(%d+)")
				rate = rate + bat_rate
				cur_cap = cur_cap + bat_cap

				if state == 0 then
					local bat_state = string.match(text, "charging state:%s*([a-z]+)")
					if bat_state == "discharging" then state = -1
					elseif bat_state == "charging" then state = 1
					end
				end

				-- Maintenant il faut lire la capacité "pleine" de la batterie
				bat = io.open(batname .. "/info")
				text = bat:read("*a")
				io.close(bat)
				bat_cap = string.match(text, "last full capacity:%s*(%d+)")
				full_cap = full_cap + bat_cap
			end
		else
			break
		end
	end

	-- Maintenant on construit la chaîne avec les infos utiles
	
	-- Est-ce qu'il y a une batterie branchée ?
	if full_cap <= 0 then
		-- On indique qu'il n'y a aucune batterie
		status = "⌁"
	else
		-- Charge de la batterie
		local charge_pc = math.floor(((cur_cap/full_cap) * 100))
	
		-- Mode de fonctionnement
		if state == 0 then
			-- Batteries idle
			status = col("yellow", charge_pc .. "% ⌁")
		else
			if state == 1 then
				status = col("green", charge_pc .. "% ↗")
			else
				local charge_color = "orange"
				if charge_pc <= 25 then charge_color = "red" end
				status = col(charge_color, charge_pc .. "% ↘")
			end

			time = cur_cap / rate
			local hour = math.floor(time)
			local min = math.floor((time - hour)*60)
			local timestr = ""

			timestr = timestr .. hour .. ":"
			if min < 10 then timestr = timestr .. "0" end
			timestr = timestr .. min

			local time_color = "lightblue"
			if (time <= .25) then time_color = "red"
			elseif (time <= .5) then time_color = "orange" end

			status = status .. col(time_color, " (" .. timestr .. ")")
		end
	end
	return status
end

