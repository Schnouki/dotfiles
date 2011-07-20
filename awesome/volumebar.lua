if gethost() == "odin-OSS4" then
   -- OSS 4

   oss_channel = "misc.front"
   oss_vol_max = 58.4
   oss_mute_1 = "misc.headphone-mix.mute.headpho"
   oss_mute_2 = "misc.lineout-mix.mute.front"

   -- Appelle un programme et renvoie la valeur lue
   function volume_cmd(cmd)
      local pipe = io.popen(cmd)
      local status = pipe:read("*all")
      io.close(pipe)
      local resultat = string.match(status, "currently set to ([0-9.ONF]+)")
      return resultat
   end

   -- Lit juste le volume
   function volume_getvol()
      local cmd = "ossmix " .. oss_channel
      local vol = volume_cmd(cmd)
      vol = tonumber(vol)
      if vol == nil then
         return nil
      else
         return (vol * 100) / oss_vol_max
      end
   end

   -- Lit juste la valeur du mode muet
   function volume_getmute()
      local cmd = "ossmix "
      local mute_1 = volume_cmd(cmd .. oss_mute_1)
      local mute_2 = volume_cmd(cmd .. oss_mute_2)
      return ((mute_1 == "ON") and (mute_2 == "ON"))
   end

   -- Lit le volume et indique si le mode muet est activé
   function volume_get()
      local vol = volume_getvol()
      local mute = volume_getmute()
      if vol == nil then return nil end
      if mute then
         vol = -vol
      end
      return vol
   end

   -- Augmentation du volume
   function volume_plus()
      local cmd = "ossmix " .. oss_channel .. " -- +" .. (0.05 * oss_vol_max)
      volume_cmd(cmd)
      return volume_get()
   end

   -- Diminution du volume
   function volume_minus()
      local cmd = "ossmix " .. oss_channel .. " -- -" .. (0.05 * oss_vol_max)
      volume_cmd(cmd)
      return volume_get()
   end

   -- Activation/désactivation du mode muet
   function volume_mute()
      local mute = volume_getmute()
      if mute then
         mute = " OFF"
      else
         mute = " ON"
      end
      local cmd_1 = "ossmix " .. oss_mute_1 .. mute
      local cmd_2 = "ossmix " .. oss_mute_2 .. mute
      volume_cmd(cmd_1)
      volume_cmd(cmd_2)
      return volume_get()
   end

else
   -- ALSA

   actrl = "Master"

   -- Appelle un programme et renvoie la valeur lue
   function volume_cmd(cmd)
      local pipe = io.popen(cmd)
      local status = pipe:read("*all")
      io.close(pipe)
      local vol = string.match(status, "%[([0-9]+)%%%]")
      local active = string.match(status, "%[(o[nf]+)%]")
      if vol == nil then
         return nil
      elseif active == "on" then
         return tonumber(vol)
      else
         return -tonumber(vol)
      end
   end

   -- Lit le volume et indique si le mode muet est activé
   function volume_get()
      local cmd = "amixer -c0 sget " .. actrl
      return volume_cmd(cmd)
   end

   -- Augmentation du volume
   function volume_plus()
      local cmd = "amixer -c0 sset " .. actrl .. " 1+"
      return volume_cmd(cmd)
   end

   -- Diminution du volume
   function volume_minus()
      local cmd = "amixer -c0 sset " .. actrl .. " 1-"
      return volume_cmd(cmd)
   end

   -- Activation/désactivation du mode muet
   function volume_mute()
      local cmd = "amixer -c0 sset " .. actrl .. " toggle"
      return volume_cmd(cmd)
   end

end

-- Mise à jour du widget
function volume_upd(widget, vol)
	local col = "#6666cc"
	if vol == nil then
		col = "#666666"
		vol = 100
	else
		vol = tonumber(vol)
		if vol < 0 then
			col = "#cc6666"
			vol = -vol
		end
	end

	widget:bar_data_add("vol", vol)
	widget:bar_properties_set("vol", {["fg"] = col})
end
