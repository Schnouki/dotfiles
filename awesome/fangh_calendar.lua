-- Fangh calendar

-- Calendrier de la Terre de Fangh, d'après l'Encyclopédie de Naheulbeuk.
-- http://encyclopedie.naheulbeuk.com/article.php3?id_article=51

local ipairs = ipairs
local os = require("os")

module("fangh_calendar")

local calendar = {
   {name="Décade de Sbroz", dur=10},
   {name="Décade de Th'ungi", dur=10},
   {name="Décade d'Uhien le Velu", dur=10},
   {name="Fête d'Honslépêle (jour du Sameule)", dur=1},

   {name="Décade de Lakkan", dur=10},
   {name="Décade de Sblortz", dur=10},
   {name="Décade maudite de Gzor", dur=10},
   {name="Décade des champignons", dur=10},
   {name="Fête de Dékhon", dur=3},

   {name="Décade de Kzaranagax l'Archi-Mage", dur=10},
   {name="Décade des géraniums en pôt", dur=10},
   {name="Décade de Phytgar Ranald", dur=10},
   {name="Fête des giboulées", dur=1},

   {name="Décade des géraniums gelés", dur=10},
   {name="Fête des grenouilles", dur=1},
   {name="Décade des pieds humides", dur=10},
   {name="Décade de Zaralbak", dur=10},
   {name="Petite Décade de Ravsgalat", dur=9},

   {name="Décade de Swimaf", dur=10},
   {name="Décade de Vontorz", dur=10},
   {name="Décade de Lasinjan", dur=10},
   {name="Décade des premières moissons", dur=10},

   {name="Fête de la bière de printemps", dur=3},
   {name="Décade de la truite", dur=10},
   {name="Décade du grand Khan Ikul", dur=10},

   {name="Fête de Dlul, jour des Feignasses, jour du sommeil, jour des couettes, jour du Cursed Pillow of Slumber", dur=1},
   {name="Décade des Grand Départs", dur=10},
   {name="Décade des Pèlerins de Dlul", dur=10},
   {name="Décade des Grands Retours", dur=10},
   {name="Fête de Oboulos", dur=1},

   {name="Décade des moissons tardives", dur=10},
   {name="Fête de la bière d'automne", dur=1},
   {name="Décade des vendanges", dur=10},
   {name="Décade des bonnes résolutions", dur=10},
   {name="Décade des Trolls", dur=10},
   {name="Fête du vin nouveau frelaté", dur=1},

   {name="Jour chômé du Mal de Tête", dur=1},
   {name="Décade de Kazarmon", dur=10},
   {name="Fête des pommes (Jour de Deuil chez les Elfes)", dur=1},
   {name="Décade des liches", dur=10},
   {name="Décade des barbares Drombards", dur=10},
   {name="Décade des barbares Moriacs", dur=10},
   {name="Décade des barbares Syldériens", dur=10},
   {name="Fête de la Baston, jour de Crom, jour pour pourrir ses voisins", dur=1},

   {name="Décade des cochons", dur=10},
   {name="Fête du gras jambon", dur=1},
   {name="Décade des boules de neige", dur=10},
   {name="Décade de Saint Taklauss", dur=10},
   {name="Jours Sacrés, fête de la Grande Binouze, Vénération hivernale de Dlul", dur=10}
}

local function get_day()
   return os.date("*t").yday
end

local function get_calendar_period(yday)
   local start = 1
   local period
   for _, period in ipairs(calendar) do
      if yday >= start and yday < (start + period.dur) then
         return {period=period, day=(yday - start + 1)}
      end
      start = start + period.dur
   end
   return nil
end

local function format_date(yday)
   local period = get_calendar_period(yday)
   if period == nil then return nil end
   if period.period.dur == 1 or period.period.name:sub(1, 5) == "Jour" then
      return period.period.name
   else
      local s
      if period.day == 1 then s = "1er" else s = "" .. period.day .. "ème" end
      s = s .. " jour de la " .. period.period.name
      return s
   end
end

function format_today()
   return format_date(get_day())
end
