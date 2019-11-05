function git_is_stashed -d "Test if there are changes in the Git stash"
    command git rev-parse --verify --quiet refs/stash > /dev/null 2>/dev/null
end
