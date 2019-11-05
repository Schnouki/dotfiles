function git_is_empty -d "Test if a repository is empty"
    git_is_repo; and test -z (command git rev-list -n 1 --all 2>/dev/null)
end
