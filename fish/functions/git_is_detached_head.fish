function git_is_detached_head -d "Test if the repository is in a detached HEAD state"
    git_is_repo; and not command git symbolic-ref HEAD 2>/dev/null > /dev/null
end
