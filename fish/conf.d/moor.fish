if command -q moor
    set -gx PAGER moor
    set -gx MOOR "--no-clear-on-exit --reformat"
end
