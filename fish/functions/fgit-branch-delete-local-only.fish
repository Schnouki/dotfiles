function fgit-branch-delete-local-only -d "Delete branches that are not tracked on any remote"
    # Regexp for "grep -P": match the local branch name, then after REMOTE: it
    # must either be:
    #   - [gone]
    #   - end-of-line ($)
    #   - a remote branch name (\S+/) that doesn't match the local one (negative
    #     lookahead on \1).

    LC_ALL=C git branch --format '%(refname:short) REMOTE:%(upstream:short)%(upstream:track)' \
        | grep -P '^(\S+) REMOTE:(\[gone\]|$|\S+/(?!\1))' \
        | cut -d" " -f1 \
        | fzf --multi --preview="git lg {}" --preview-window=up:75% --prompt="Delete branches> " --layout=reverse \
        | xargs --no-run-if-empty git branch --delete --force
end
