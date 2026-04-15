local wezterm = require 'wezterm'
local actions = wezterm.action
local config = wezterm.config_builder()

local is_linux = wezterm.target_triple:find("linux") ~= nil

---- Appearance Settings -----

config.color_scheme = 'Catppuccin Mocha'
-- config.color_scheme = 'Catppuccin Latte'
-- config.color_scheme = 'Catppuccin Macchiato'

-- Disable ligatures
-- https://wezfurlong.org/wezterm/config/font-shaping.html#advanced-font-shaping-options
-- config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }

if is_linux then
    config.font_size = 12.0
else
    config.font_size = 17.0
end

config.window_frame = {
    font = wezterm.font({ family = 'JetBrains Mono', weight = 'Bold' }),
    font_size = config.font_size - 2.0,
}

config.window_padding = {
    left = 0,
    right = 0,
    top = 0,
    bottom = 0,
}

if is_linux then
    config.window_decorations = "NONE"
else
    config.window_decorations = "RESIZE"
end

config.hide_tab_bar_if_only_one_tab = true
-- config.tab_bar_at_bottom = false
-- config.use_fancy_tab_bar = false

config.enable_scroll_bar = false

---- Key Bindings ----

local keys = {
    {
        key = 'F11',
        action = actions.ToggleFullScreen
    }
}

config.leader = { key = 'l', mods = 'CTRL', timeout_milliseconds = 1000 }

local leader_keys = {
    { key = 'z', action = actions.TogglePaneZoomState },
    { key = 'v', action = actions.SplitHorizontal { domain = 'CurrentPaneDomain' } },
    { key = 'S', mods = 'SHIFT',                                                   action = actions.SplitVertical { domain = 'CurrentPaneDomain' } },
    { key = 'h', action = actions.ActivatePaneDirection 'Left' },
    { key = 'j', action = actions.ActivatePaneDirection 'Down' },
    { key = 'k', action = actions.ActivatePaneDirection 'Up' },
    { key = 'l', action = actions.ActivatePaneDirection 'Right' },
    { key = 'x', action = actions.CloseCurrentPane { confirm = false } },
    { key = 't', action = actions.SpawnTab 'CurrentPaneDomain' },
    { key = 'c', action = actions.SpawnTab 'CurrentPaneDomain' },
    { key = 'n', action = actions.ActivateTabRelative(1) },
    { key = 'p', action = actions.ActivateTabRelative(-1) },
    { key = '1', action = actions.ActivateTab(0) },
    { key = '2', action = actions.ActivateTab(1) },
    { key = '3', action = actions.ActivateTab(2) },
    { key = '4', action = actions.ActivateTab(3) },
    { key = '5', action = actions.ActivateTab(4) },
    { key = '6', action = actions.ActivateTab(5) },
    { key = '7', action = actions.ActivateTab(6) },
    { key = '8', action = actions.ActivateTab(7) },
    { key = '9', action = actions.ActivateLastTab },
}

for _, key in ipairs(leader_keys) do
    key.mods = key.mods and (key.mods .. '|LEADER') or 'LEADER'
    table.insert(keys, key)
end

if is_linux then
    for i = 1, 9 do
        -- alt + number to activate that tab
        table.insert(keys, {
            key = tostring(i),
            mods = 'ALT',
            action = actions.ActivateTab(i - 1),
        })
    end
end

-- Rather than emitting fancy composed characters when alt is pressed, treat the
-- input more like old school ascii with ALT held down
config.send_composed_key_when_left_alt_is_pressed = false
config.send_composed_key_when_right_alt_is_pressed = false

config.keys = keys
config.mouse_bindings = {
    -- Ctrl-click will open the link under the mouse cursor
    {
        event = { Up = { streak = 1, button = 'Left' } },
        mods = 'CTRL',
        action = wezterm.action.OpenLinkAtMouseCursor,
    },
}

-- Maximise the window on startup
wezterm.on('gui-startup', function()
    local _, _, window = wezterm.mux.spawn_window({})
    window:gui_window():maximize()
end)

local fish_path = ''
if is_linux then
    fish_path = wezterm.glob('/usr/bin/fish')
else
    fish_path = wezterm.glob('/opt/homebrew/bin/fish')
end

fish_path = fish_path[1]
if fish_path ~= nil then
    config.default_prog = { fish_path, '-l' }
else
    config.default_prog = { 'fish', '-l' }
end

config.warn_about_missing_glyphs = false

return config
