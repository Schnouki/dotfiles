function git_is_dirty -d "Test if there are changes not staged for commit"
    git_is_repo; and not command git diff --no-ext-diff --quiet --exit-code 2>/dev/null
end
