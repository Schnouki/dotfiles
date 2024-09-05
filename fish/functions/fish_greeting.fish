function fish_greeting --description 'Show a greeting'
    if type -q sfortune
        and not set -q DISABLE_SFORTUNE ASCIINEMA_REC
        sfortune
    end
end
