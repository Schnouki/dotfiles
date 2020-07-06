function fish_greeting --description 'Show a greeting'
    if type -q sfortune
        sfortune
    end
end
