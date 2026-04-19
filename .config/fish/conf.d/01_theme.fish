# Load a fish theme that matches the current light/dark appearance.

function __fish_terminal_appearance --description 'Detect the current terminal appearance'
    if set -q fish_terminal_appearance
        switch (string lower -- $fish_terminal_appearance)
            case dark light
                echo $fish_terminal_appearance
                return
        end
    end

    if test (uname) = Darwin
        if defaults read -g AppleInterfaceStyle >/dev/null 2>&1
            echo dark
        else
            echo light
        end
        return
    end

    if command -q gsettings
        set -l scheme (gsettings get org.gnome.desktop.interface color-scheme 2>/dev/null)
        switch $scheme
            case "*prefer-dark*"
                echo dark
                return
            case "*prefer-light*" "*default*"
                echo light
                return
        end
    end

    echo dark
end

function __fish_sync_theme --description 'Sync fish colors with terminal appearance'
    set -l appearance (__fish_terminal_appearance)
    if test "$appearance" = "$__fish_active_theme"
        return
    end

    set -l theme_dir "$XDG_CONFIG_HOME/fish/themes"
    set -l theme_file "$theme_dir/carbonfox.theme"
    if test "$appearance" = light
        set theme_file "$theme_dir/dayfox.theme"
    end

    if test -f "$theme_file"
        source "$theme_file"
        set -g __fish_active_theme "$appearance"
    end
end

__fish_sync_theme
