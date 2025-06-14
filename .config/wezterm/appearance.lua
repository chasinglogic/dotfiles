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

    -- This hides status info that is useful so always show it.
    config.hide_tab_bar_if_only_one_tab = false
    config.tab_bar_at_bottom = false
    config.use_fancy_tab_bar = false

    config.enable_scroll_bar = false
    config.window_padding = {
        left = 0,
        right = 0,
        bottom = 0,
        top = '0.2cell',
    }
end

local function segments_for_right_status(window)
    return {
        window:active_workspace(),
        wezterm.strftime('%a %b %-d %H:%M'),
        wezterm.hostname(),
    }
end

wezterm.on('update-status', function(window, _)
    local segments = segments_for_right_status(window)

    local color_scheme = window:effective_config().resolved_palette
    -- Note the use of wezterm.color.parse here, this returns
    -- a Color object, which comes with functionality for lightening
    -- or darkening the colour (amongst other things).
    local bg = wezterm.color.parse(color_scheme.background)
    local fg = color_scheme.foreground

    -- Each powerline segment is going to be coloured progressively
    -- darker/lighter depending on whether we're on a dark/light colour
    -- scheme. Let's establish the "from" and "to" bounds of our gradient.
    local gradient_to, gradient_from = bg
    gradient_from = gradient_to:lighten(0.2)

    -- Yes, WezTerm supports creating gradients, because why not?! Although
    -- they'd usually be used for setting high fidelity gradients on your terminal's
    -- background, we'll use them here to give us a sample of the powerline segment
    -- colours we need.
    local gradient = wezterm.color.gradient(
        {
            orientation = 'Horizontal',
            colors = { gradient_from, gradient_to },
        },
        #segments -- only gives us as many colours as we have segments.
    )

    -- We'll build up the elements to send to wezterm.format in this table.
    local elements = {}

    for i, seg in ipairs(segments) do
        local is_first = i == 1
        local is_last = i == #segments

        if is_first then
            table.insert(elements, { Background = { Color = 'none' } })
        end

        table.insert(elements, { Foreground = { Color = fg } })
        table.insert(elements, { Background = { Color = gradient[i] } })
        table.insert(elements, { Text = ' ' .. seg .. ' ' })

        if is_last then
            table.insert(elements, { Text = ' ' })
        end
    end

    window:set_right_status(wezterm.format(elements))
end)

-- local function tab_title(tab_info)
--     local title = tab_info.tab_title
--     -- if the tab title is explicitly set, take that
--     if title and #title > 0 then
--         return title
--     end
--     -- Otherwise, use the process name of the active pane
--     -- in that tab
--     local procname = tab_info.active_pane.foreground_process_name
--     if procname == "" then
--         return "uknown"
--     end

--     return procname
-- end

-- Tried playing with this, I have to return everythig not just the title of the
-- tab. so needs more playing then I have time for right now
-- wezterm.on(
--     'format-tab-title',
--     function(tab, tabs, panes, config, hover, max_width)
--         tab.title = tab_title(tab)
--         return tab
--     end
-- )

return module
