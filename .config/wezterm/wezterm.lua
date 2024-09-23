local wezterm = require 'wezterm'
local config = wezterm.config_builder()
local sys = require('sys')

require('appearance').apply_to_config(config)
require('keys').apply_to_config(config)

-- Maximise the window on startup
wezterm.on('gui-startup', function()
    local _, _, window = wezterm.mux.spawn_window({})
    window:gui_window():maximize()
end)

return config
