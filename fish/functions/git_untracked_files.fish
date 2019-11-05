function git_untracked_files -d "Get the number of untracked files in a repository"
    git_is_repo; and command git ls-files --others --exclude-standard | command awk '

        BEGIN {
            n = 0
        }

        { n++ }

        END {
            print n
            exit !n
        }
    '
end
