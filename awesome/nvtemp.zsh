#!/bin/zsh

setopt RE_MATCH_PCRE

nvidia-settings -q gpus | while read line; do
    if [[ "$line" =~ "(\[gpu:\d+\])" ]]; then
        nvidia-settings -q "${match}/GPUCoreTemp" -t
    fi
done
