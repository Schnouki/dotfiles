function ll --wraps='ls -FNhl --color=auto' --wraps=exa_git --description 'alias ll exa_git'
  exa_git $argv
        
end
