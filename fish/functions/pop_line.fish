function pop_line -d "Pop the previous pushed buffer and load it into current buffer"
    if not count $__push_line_stack >/dev/null
        return 1
    end

    set -l __line (string split -m 1 ":" $__push_line_stack[1])
    set -e __push_line_stack[1]
    commandline "$__line[2]"
    commandline -C $__line[1]
end
