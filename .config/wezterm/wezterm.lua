local wezterm = require 'wezterm'
local config = wezterm.config_builder()

require('keys').apply_to_config(config)

config.color_scheme = 'Dracula'
config.font = wezterm.font 'Hack'
config.font_size = 17.0
config.line_height = 1.1

-- This hides status info that is useful so always show it.
config.hide_tab_bar_if_only_one_tab = false
config.tab_bar_at_bottom = true
-- Makes it look more "terminaly" instead of trying to look "native" to the
-- Platform it is on and failing.
config.use_fancy_tab_bar = false

config.enable_scroll_bar = false
config.window_padding = {
    left = 0,
    right = 0,
    bottom = 0,
    -- A little separation between tab bar and terminal.
    top = "0.2cell",
}

-- Maximise the window on startup
wezterm.on('gui-startup', function()
    local _, _, window = wezterm.mux.spawn_window({})
    window:gui_window():maximize()
end)

return config
