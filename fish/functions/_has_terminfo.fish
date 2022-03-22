function _has_terminfo -d "Check if a terminfo file exists for $term" -a term
    set -l t1 (string sub -e 1 "$term")
    set -l filename "$t1/$term"
    return (test -e "~/.terminfo/$filename" -o -e "/usr/share/terminfo/$filename")
end
