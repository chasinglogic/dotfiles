local wezterm = require 'wezterm'
local sys = require 'sys'
local module = {}

function module.apply_to_config(config)
    config.color_scheme = 'Catppuccin Mocha'

    -- Disable ligatures
    -- https://wezfurlong.org/wezterm/config/font-shaping.html#advanced-font-shaping-options
    -- config.harfbuzz_features = { 'calt=0', 'clig=0', 'liga=0' }

    if sys.is_os('linux') then
        config.font_size = 14.0
    else
        config.font_size = 18.0
    end
    config.line_height = 1.1

    config.window_frame = {
        font = wezterm.font({ family = 'JetBrains Mono', weight = 'Bold' }),
        font_size = config.font_size - 2.0,
    }

    if sys.is_linux then
        config.window_decorations = "NONE"
    else
        config.window_decorations = "RESIZE"
    end

    config.hide_tab_bar_if_only_one_tab = true
    -- config.tab_bar_at_bottom = false
    -- config.use_fancy_tab_bar = false

    config.enable_scroll_bar = false
    config.window_padding = {
        left = 0,
        right = 0,
        bottom = 0,
        top = '0.2cell',
    }
end

return module
