function fish-reload-config
    for file in $XDG_CONFIG_HOME/fish/**/*.fish
        source $file
    end
end
