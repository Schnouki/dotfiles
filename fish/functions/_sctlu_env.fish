function _sctlu_env -d "Import relevant environment variables in systemctl --user"
    # Variables known to be imported
    set -l vars PATH XDG_DATA_HOME XDG_STATE_HOME XDG_CONFIG_HOME XDG_CACHE_HOME

    # Read more variables from xdg.fish
    set --append vars (awk '/^set -Ux/ {print $3}' ~/.config/fish/conf.d/xdg.fish)

    systemctl --user import-environment $vars
end
