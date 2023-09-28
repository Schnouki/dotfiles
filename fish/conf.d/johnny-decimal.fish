set -g JD_DIR ~/Documents

function __jd --description "Convert a Johnny.Decimal ID to a path" -a id
    echo $JD_DIR/**/$id\ *
end

function jdcd --description "Change directory to a Johnny.Decimal folder" -a id
    cd (__jd $id)
end

function jdcp --description "Copy files to a Johnny.Decimal folders"
    cp $argv[2..] (__jd $argv[1])/
end

function jdmv --description "Move files to a Johnny.Decimal folders"
    mv $argv[2..] (__jd $argv[1])/
end

function __jd_complete --description "Output completions for a Johnny.Decimal ID" -a id
    for line in (find $JD_DIR/ -mindepth 2 -maxdepth 3 \( -type d -and -name "$id*" \) -printf '/%P\0' | sort -z | string split0)
        set -l line_desc (string match --all --groups-only --regex '/[0-9.-]+\s*([^/]+)' "$line" | string join ' > ')
        set -l line_id (string match --groups-only --regex '/([0-9.-]+)\s*[^/]+$' "$line")
        printf "$line_id\t$line_desc\n"
    end
end

complete -c jdcd -xa "(__jd_complete)"
complete -c jdcp -n __fish_is_first_arg -fa "(__jd_complete)"
complete -c jdmv -n __fish_is_first_arg -fa "(__jd_complete)"
