local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.color_scheme = 'Dracula'
config.font = wezterm.font 'Hack'
config.font_size = 17.0
config.line_height = 1.1
config.hide_tab_bar_if_only_one_tab = true

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
