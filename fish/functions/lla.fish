function lla --wraps='ls -FNhla --color=auto' --wraps='exa_git $EXA_LA_OPTIONS' --description 'alias lla exa_git $EXA_LA_OPTIONS'
  exa_git $EXA_LA_OPTIONS $argv
        
end
