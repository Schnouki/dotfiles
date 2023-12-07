function fgit-branch-delete-local-only -d "Delete branches that are not tracked on any remote"
    LC_ALL=C git branch --format '%(refname:short) REMOTE:%(upstream:short)%(upstream:track)' \
        | egrep 'REMOTE:((.*\[gone\]))?$' \
        | cut -d" " -f1 \
        | fzf --multi --preview="git lg {}" --preview-window=up:75% --prompt="Delete branches> " --layout=reverse \
        | xargs --no-run-if-empty git branch --delete --force
end
