function exists(filename)
   local file = io.open(filename)
   if file then
      io.close(file)
      return true
   else
      return false
   end
end

function file_as_number(filename)
   local file = io.open(filename)
   if file then
      local n = file:read("*n")
      io.close(file)
      return n
   else
      return -1
   end
end

function file_as_string(filename)
   local file = io.open(filename)
   if file then
      local s = file:read("*l")
      io.close(file)
      return s
   else
      return ""
   end
end

-- Texte en couleur
function col(color, text)
   return "<span color=\"" .. color .. "\">" .. text .. "</span>"
end

-- État des batteries
function battery_mon()
   local state = 0 -- 0: idle, 1: charge, -1: discharge
   local energy_full = 0
   local energy_now = 0
   local power_now = 0

   local i = 0

   while true do
      -- Est-ce que la batterie n° i existe ?
      local batname = "/sys/class/power_supply/BAT" .. i
      i = i+1
      if exists(batname) then
         -- Est-ce que la batterie est là ?
         local present = file_as_number(batname .. "/present")
         if present == 1 then
            local bat_state = file_as_string(batname .. "/status")
            local read_cap = false
            if bat_state == "Charging" then
               state = 1
               read_cap = true
            elseif bat_state == "Discharging" then
               state = -1
               read_cap = true
            end

            -- Lecture de l'état actuel
            energy_full = energy_full + file_as_number(batname .. "/energy_full")
            energy_now  = energy_now  + file_as_number(batname .. "/energy_now")
            power_now   = power_now   + file_as_number(batname .. "/power_now")
         end
      else
         break
      end
   end

   -- Maintenant on construit la chaîne avec les infos utiles
   local status = ""

   -- Est-ce qu'il y a une batterie branchée ?
   if energy_full <= 0 then
      -- On indique qu'il n'y a aucune batterie
      status = "⌁"
   else
      -- Charge de la batterie
      local charge_pc = math.floor(((energy_now/energy_full) * 100))

      -- Mode de fonctionnement
      if state == 0 then
         -- Batteries idle
         status = col("yellow", charge_pc .. "% ⌁")
      else
         local energy_left = 0

         if state == 1 then
            status = col("green", charge_pc .. "% ↗")
            energy_left = energy_full - energy_now
         else
            local charge_color = "orange"
            if charge_pc <= 25 then charge_color = "red" end
            status = col(charge_color, charge_pc .. "% ↘")
            energy_left = energy_now
         end

         -- Temps restant.
         -- Les energy_* sont en µWh, power_now est en µW.
         local time = energy_left / power_now -- Temps en *heures*
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
