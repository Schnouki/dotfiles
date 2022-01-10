function fdocker-rmi -d "Remove Docker images"
    docker images | fzf --header-lines=1 --multi --layout=reverse | awk '{print $3}' | xargs -r docker rmi
end
