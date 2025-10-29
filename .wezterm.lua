local wezterm = require 'wezterm'
local actions = wezterm.action
local config = wezterm.config_builder()

local function is_os(name)
    return wezterm.target_triple:find(name) ~= nil
end

local is_linux = is_os("linux")

---- Appearance Settings -----

-- config.color_scheme = 'Catppuccin Mocha'
-- config.color_scheme = 'Catppuccin Latte'
config.color_scheme = 'Catppuccin Macchiato'

-- Disable ligatures
-- https://wezfurlong.org/wezterm/config/font-shaping.html#advanced-font-shaping-options
-- config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }

if is_linux then
    config.font_size = 13.0
else
    config.font_size = 17.0
end
config.line_height = 1.1

config.window_frame = {
    font = wezterm.font({ family = 'JetBrains Mono', weight = 'Bold' }),
    font_size = config.font_size - 2.0,
}

local padding_size = "1cell"
config.window_padding = {
    left = padding_size,
    right = 0,
    top = '0.5cell',
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
}

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
end

config.warn_about_missing_glyphs = false

return config
