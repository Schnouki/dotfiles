alias lx="l --sort time"
alias lxr="lx --reverse"
alias llx="ll --sort time"
alias llxr="llx --reverse"

alias df='df -h'
alias free='free -m'

alias grep='grep --color=auto'
alias egrep='grep -E --color=auto'

alias ip="ip --color=auto"

alias ssu="sudo -i"

alias sctl="systemctl"
alias sctlu="systemctl --user"
alias jctl="journalctl"
alias jctlu="journalctl --user"

alias ec="eclient -n -c"
alias ecn="eclient -n"
alias ecnw="eclient -nw"

alias zbarscreen="zbarimg (maim -s | psub)"

# yt-dlp with specific config file
alias ytdlp-music="yt-dlp --config-location ~/.config/yt-dlp/music"
alias ytdlp-ytmusic="yt-dlp --config-location ~/.config/yt-dlp/ytmusic"

# Wine 32-bit
alias wine32="WINEPREFIX=$XDG_DATA_HOME/wine32 WINEARCH=win32 wine"
alias wineconsole32="WINEPREFIX=$XDG_DATA_HOME/wine32 WINEARCH=win32 wineconsole"
alias winetricks32="WINEPREFIX=$XDG_DATA_HOME/wine32 WINEARCH=win32 winetricks"

# use --bhelp to colorize --help with bat
abbr --add --position anywhere --set-cursor -- --bhelp "--help % | bat -plhelp"

alias llm_tldr="llmcli -q \"What's the tl;dr version of this?\" -f"
