function fdocker-rm -d "Remove Docker containers"
    docker ps -a | fzf --header-lines=1 --multi --layout=reverse --print0 | cut -z -d" " -f1 | xargs -r0 docker rm
end
