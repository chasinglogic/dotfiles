local wezterm = require 'wezterm'
local actions = wezterm.action
local module = {}

function module.apply_to_config(config)
    local keys = {
    }

    for i = 1, 9 do
        -- LEADER + number to activate that tab
        -- table.insert(keys, {
        --     key = tostring(i),
        --     mods = 'LEADER',
        --     action = actions.ActivateTab(i - 1),
        -- })

        -- alt + number to activate that tab
        table.insert(keys, {
            key = tostring(i),
            mods = 'ALT',
            action = actions.ActivateTab(i - 1),
        })
    end

    -- Rather than emitting fancy composed characters when alt is pressed, treat the
    -- input more like old school ascii with ALT held down
    config.send_composed_key_when_left_alt_is_pressed = false
    config.send_composed_key_when_right_alt_is_pressed = false

    -- config.leader = { key = 'l', mods = 'CTRL', timeout_milliseconds = 1000 }
    config.keys = keys
    config.mouse_bindings = {
        -- Ctrl-click will open the link under the mouse cursor
        {
            event = { Up = { streak = 1, button = 'Left' } },
            mods = 'CTRL',
            action = wezterm.action.OpenLinkAtMouseCursor,
        },
    }
end

return module
