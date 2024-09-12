local wezterm = require 'wezterm'
local config = wezterm.config_builder()
local sys = require('sys')

-- if sys.file_exists('/opt/homebrew/bin/nu') then
--     config.default_prog = { '/opt/homebrew/bin/nu', '-l' }
-- end

require('appearance').apply_to_config(config)
require('keys').apply_to_config(config)

-- Maximise the window on startup
wezterm.on('gui-startup', function()
    local _, _, window = wezterm.mux.spawn_window({})
    window:gui_window():maximize()
end)

return config
