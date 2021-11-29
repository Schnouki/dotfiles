function fgh-delete-merged-branches -d "Delete branches that are associated with merged PRs"
    join \
        (gh pr list --state merged --limit 400 --json id,baseRefName,headRefName,number,title --template '{{range .}}{{.headRefName}} -- {{.title}} (#{{.number}}){{ "\n" }}{{end}}' \
        | sort -k 1b,1 | psub) \
        (git branch --format '%(refname:short)' | sort -k 1b,1 | psub) \
        | fzf --prompt="Delete branches> " --layout=reverse-list --multi --preview="CLICOLOR_FORCE=1 gh pr view {1}" --bind 'ctrl-a:select-all' \
        | cut -d" " -f1 \
        | xargs --no-run-if-empty git branch --delete --force
end
