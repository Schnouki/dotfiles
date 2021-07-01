function tweechat -d "Start or join a weechat session in tmux"
    tmux new-session -A -s weechat -c "$HOME" weechat
end
