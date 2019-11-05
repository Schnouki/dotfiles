function git_is_staged -d "Test if there are changes staged for commit"
    git_is_repo; and not command git diff --cached --no-ext-diff --quiet --exit-code 2>/dev/null
end
