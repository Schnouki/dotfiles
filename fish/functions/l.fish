function l --wraps='ls -FNh --color=auto' --wraps='exa $EXA_STANDARD_OPTIONS $EXA_L_OPTIONS' --description 'alias l exa $EXA_STANDARD_OPTIONS $EXA_L_OPTIONS'
  exa $EXA_STANDARD_OPTIONS $EXA_L_OPTIONS $argv
        
end
