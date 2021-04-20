(local lfs (require :lfs))

(fn get-all-acf [path]
    "List all acf app manifests in PATH."
    (icollect [filename (lfs.dir path)]
              (when (string.match filename ".acf$")
                (.. path "/" filename))))

(fn parse-acf [filename]
    "Parse an acf app manifest to extract the game name and appid."
    (collect
     [line (io.lines filename)]
     (match (string.match line "%s*\"(%w+)\"%s+\"([^\"]+)\"")
            (:appid v) (values :id v)
            (:name v) (values :name v))))

(fn get-games [path]
    "Get a sorted list of all games in PATH."
    (doto (icollect
           [_ filename (ipairs (get-all-acf path))]
           (parse-acf filename))
      (table.sort #(< $1.name $2.name))))

{
 :get_games get-games
}
