# All supported subcommands -- from `timew -h`
set -l commands annotate cancel config continue day delete diagnostics export extensions gaps get help join lengthen modify month move report shorten show split start stop summary tag tags track undo untag week

function __timew_complete_tags
    jq -r "keys | .[]" ~/.timewarrior/data/tags.data
end

function __timew_complete_ids
    timew summary :month :ids | python3 -c "import sys; idxs = []
for line in sys.stdin:
    if line.startswith('--'):
        idxs = [n for n, c in enumerate(line) if c == ' ']; continue
    if idxs:
        if not line.strip(): break
        print(f'{line[idxs[2]:idxs[3]].strip()}\t{line[idxs[3]:idxs[4]].strip()} ({line[idxs[6]:idxs[7]].strip()})')"
end

# Disable file completions
complete -c timew -f

# All commands
complete -c timew -n "not __fish_seen_subcommand_from $commands" -a "$commands"

complete -c timew -n __fish_use_subcommand -a start -d "Start time tracking"
complete -c timew -n __fish_use_subcommand -a stop -d "Stop time tracking"
complete -c timew -n __fish_use_subcommand -a track -d "Add intervals to the database"
complete -c timew -n "__fish_seen_subcommand_from start stop track" -xa "(__timew_complete_tags)"

complete -c timew -n __fish_use_subcommand -a join -d "Join two intervals"
complete -c timew -n __fish_use_subcommand -a split -d "Split an interval in two"
complete -c timew -n "__fish_seen_subcommand_from join split" -xa "(__timew_complete_ids)"

complete -c timew -n __fish_use_subcommand -a continue -d "Resume tracking of existing interval"
complete -c timew -n "__fish_seen_subcommand_from continue" -xa "(__timew_complete_tags; __timew_complete_ids)"

complete -c timew -n __fish_use_subcommand -a cancel -d "Cancel time tracking"
complete -c timew -n __fish_use_subcommand -a undo -d "Undo last command"

# Boring stuff
complete -c timew -s h -l help -d "Help screen"
complete -c timew -l version -d "Show version"
