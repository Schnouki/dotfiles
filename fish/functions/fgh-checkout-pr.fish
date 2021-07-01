function fgh-checkout-pr -d "Checkout a GitHub PR"
    set -l pr_number (gh pr list | fzf --prompt="Checkout PR> " --preview="CLICOLOR_FORCE=1 gh pr view {1}" | cut -f1)
    if test -n $pr_number
        gh pr checkout $pr_number
    end
end
