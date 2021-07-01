(local lfs (require :lfs))

(fn get-all-acf [steamdir]
  "List all acf app manifests in STEAMDIR."
  (let [path (.. steamdir "/" "steamapps")]
    (icollect [filename (lfs.dir path)]
      (when (string.match filename ".acf$")
        (.. path "/" filename)))))

(fn parse-acf [filename]
  "Parse an acf app manifest to extract the game name and appid."
  (collect
      [line (io.lines filename)]
    (match (string.match line "%s*\"(%w+)\"%s+\"([^\"]+)\"")
      (:appid v) (values :id v)
      (:name v) (values :name v))))

(fn get-game-icon [steamdir game]
  (.. steamdir "/appcache/librarycache/" (tostring game.id) "_icon.jpg"))

(fn get-game [steamdir acf-filename]
  (let [game (parse-acf acf-filename)]
    (tset game :icon (get-game-icon steamdir game))
    game))

(fn get-games [steamdir]
  "Get a sorted list of all games in STEAMDIR."
  (doto
      (icollect [_ filename (ipairs (get-all-acf steamdir))]
        (get-game steamdir filename))
    (table.sort #(< $1.name $2.name))))

{
 :get_games get-games
}
