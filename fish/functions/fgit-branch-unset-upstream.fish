function fgit-branch-unset-upstream -d "Unset upstream for many branches" --argument-names upstream
    if test -z "$upstream"
        echo "Argument needed: target upstream" >&2
        commandline --function repaint
        return 22
    end
    LC_ALL=C git branch --format '%(refname:short) REMOTE:%(upstream:short)' \
        | egrep "REMOTE:$upstream\$" \
        | cut -d" " -f1 \
        | fzf --multi --preview="git lg {}" --preview-window=up:75% --prompt="Unset upstream> " --layout=reverse \
        | xargs --no-run-if-empty --max-args=1 git branch --unset-upstream
end
