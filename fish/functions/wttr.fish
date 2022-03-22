function wttr -a location -d "Show weather forecast"
    if test -z "$location"
        set location (cat ~/.config/wttr_location | string trim)
    end
    curl -fGsS "https://fr.wttr.in/$location?Fq"
end
