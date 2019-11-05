function git_is_touched -d "Test if there are any changes in the working tree"
    git_is_staged; or git_is_dirty
end
