# Herdr only accepts OSC 7 with an empty or "localhost" host; fish sends $hostname by default.
if set -q HERDR_ENV
    function __fish_update_cwd_osc --on-variable=PWD --on-event=fish_prompt
        printf \e\]7\;file://%s\a (string escape --style=url -- $PWD)
    end
end
