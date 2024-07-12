local wezterm = require 'wezterm'
local sys = require 'sys'
local module = {}

function module.apply_to_config(config)
    config.color_scheme = 'Dracula (Official)'
    -- config.color_scheme = 'Solarized Light (Gogh)'
    config.font = wezterm.font 'Hack'
    if sys.is_os('linux') then
        config.font_size = 13.0
    else
        config.font_size = 17.0
    end
    config.line_height = 1.1

    config.window_decorations = "RESIZE"

    -- This hides status info that is useful so always show it.
    config.hide_tab_bar_if_only_one_tab = false
    config.tab_bar_at_bottom = true
    -- -- Makes it look more "terminaly" instead of trying to look "native" to the
    -- -- Platform it is on and failing.
    config.use_fancy_tab_bar = false

    config.enable_scroll_bar = false
    config.window_padding = {
        left = 0,
        right = 0,
        bottom = 0,
        top = '0.2cell',
    }

    config.colors = {
        tab_bar = {
            -- The color of the strip that goes along the top of the window
            -- (does not apply when fancy tab bar is in use)
            background = '#282A36',

            -- The active tab is the one that has focus in the window
            active_tab = {
                -- The color of the background area for the tab
                bg_color = '#44475A',
                -- The color of the text for the tab
                fg_color = '#F8F8F2',

                -- Specify whether you want "Half", "Normal" or "Bold" intensity for the
                -- label shown for this tab.
                -- The default is "Normal"
                intensity = 'Normal',

                -- Specify whether you want "None", "Single" or "Double" underline for
                -- label shown for this tab.
                -- The default is "None"
                underline = 'Single',

                -- Specify whether you want the text to be italic (true) or not (false)
                -- for this tab.  The default is false.
                italic = false,

                -- Specify whether you want the text to be rendered with strikethrough (true)
                -- or not for this tab.  The default is false.
                strikethrough = false,
            },

            -- Inactive tabs are the tabs that do not have focus
            inactive_tab = {
                bg_color = '#282A36',
                fg_color = '#F8F8F2',

                -- The same options that were listed under the `active_tab` section above
                -- can also be used for `inactive_tab`.
            },

            -- You can configure some alternate styling when the mouse pointer
            -- moves over inactive tabs
            inactive_tab_hover = {
                bg_color = '#44475A',
                fg_color = '#F8F8F2',

                -- The same options that were listed under the `active_tab` section above
                -- can also be used for `inactive_tab_hover`.
            },

            -- The new tab button that let you create new tabs
            new_tab = {
                bg_color = '#282A36',
                fg_color = '#8BE9FD',
                intensity = 'Bold',

                -- The same options that were listed under the `active_tab` section above
                -- can also be used for `new_tab`.
            },

            -- You can configure some alternate styling when the mouse pointer
            -- moves over the new tab button
            new_tab_hover = {
                bg_color = '#44475A',
                fg_color = '#8BE9FD',
                intensity = 'Bold',

                -- The same options that were listed under the `active_tab` section above
                -- can also be used for `new_tab_hover`.
            },
        },
    }
end

return module
