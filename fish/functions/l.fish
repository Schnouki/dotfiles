function l --wraps='ls -FNh --color=auto' --wraps='exa $EXA_STANDARD_OPTIONS $EXA_L_OPTIONS' --wraps='eza $EXA_STANDARD_OPTIONS $EXA_L_OPTIONS' --description 'alias l eza $EXA_STANDARD_OPTIONS $EXA_L_OPTIONS'
  eza $EXA_STANDARD_OPTIONS $EXA_L_OPTIONS $argv
        
end
