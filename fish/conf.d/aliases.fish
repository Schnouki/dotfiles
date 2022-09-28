alias ls='ls -FNh --color=auto'
alias l='ls -FNh --color=auto'
alias lk='ls -FNh --color=auto'
alias la='ls -FNha --color=auto'
alias ll='ls -FNhl --color=auto'
alias lla='ls -FNhla --color=auto'

alias df='df -h'
alias free='free -m'

alias grep='grep --color=auto'
alias egrep='egrep --color=auto'

alias ssu="sudo -i"

alias sctl="systemctl"
alias sctlu="systemctl --user"
alias jctl="journalctl"
alias jctlu="journalctl --user"

alias ec="eclient -n -c"
alias ecn="eclient -n"
alias ecnw="eclient -nw"

alias ea="exa -F --group-directories-first"
alias el="ea -l --git --time-style long-iso"
alias eaa="ea -a"
alias ela="el -a"
alias elt="el -s modified"
alias elta="elt -a"

alias zbarscreen="zbarimg (maim -s | psub)"

# youtube-dl with specific config file
alias ytmusicdl="youtube-dl --config-location ~/.config/youtube-dl/music"

# Wine 32-bit
alias wine32="WINEPREFIX=$HOME/.wine32 WINEARCH=win32 wine"
alias wineconsole32="WINEPREFIX=$HOME/.wine32 WINEARCH=win32 wineconsole"
alias winetricks32="WINEPREFIX=$HOME/.wine32 WINEARCH=win32 winetricks"
