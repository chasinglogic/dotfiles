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

local fish_path = ''
if sys.is_linux then
    -- Not sure this is correct yet
    fish_path = wezterm.glob('/usr/bin/fish')
else
    fish_path = wezterm.glob('/opt/homebrew/bin/fish')
end

fish_path = fish_path[1]
if fish_path ~= nil then
    config.default_prog = { fish_path, '-l' }
end

return config
