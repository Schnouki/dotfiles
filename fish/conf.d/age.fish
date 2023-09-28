function age-enc -d "encrypt with age" -w age
    set -l args ()
    for file in ~/.config/age/age-*.pub
        set -a args -R "$file"
    end
    age --encrypt $args $argv
end

function age-dec -d "decrypt with age" -w age
    set -l args ()
    for file in ~/.config/age/*-identity-*.txt
        set -a args -i "$file"
    end
    age --decrypt $args $argv
end
