local wezterm = require 'wezterm'
local projects = require 'projects'
local sys = require 'sys'
local actions = wezterm.action
local module = {}

function module.apply_to_config(config)
    local keys = {
        -- Tmux emulation bindings
        {
            key = 'r',
            mods = 'LEADER',
            action = actions.ReloadConfiguration,
        },
        {
            key = 'S',
            mods = 'LEADER',
            action = actions.SplitVertical,
        },
        {
            key = 'v',
            mods = 'LEADER',
            action = actions.SplitHorizontal,
        },
        {
            key = 'c',
            mods = 'LEADER',
            action = actions.SpawnTab('CurrentPaneDomain'),
        },
        {
            key = 'n',
            mods = 'LEADER',
            action = actions.ActivateTabRelative(1),
        },
        {
            key = 'p',
            mods = 'LEADER',
            action = actions.ActivateTabRelative(-1),
        },
        {
            key = 'h',
            mods = 'LEADER',
            action = actions.ActivatePaneDirection('Left'),
        },
        {
            key = 'j',
            mods = 'LEADER',
            action = actions.ActivatePaneDirection('Down'),
        },
        {
            key = 'k',
            mods = 'LEADER',
            action = actions.ActivatePaneDirection('Up'),
        },
        {
            key = 'l',
            mods = 'LEADER',
            action = actions.ActivatePaneDirection('Right'),
        },
        {
            key = 'x',
            mods = 'LEADER',
            action = actions.CloseCurrentPane({ confirm = false }),
        },
        {
            key = 'm',
            mods = 'LEADER',
            action = wezterm.action_callback(function(win, pane)
                local tab = win:active_tab()
                for _, p in ipairs(tab:panes()) do
                    if p:pane_id() ~= pane:pane_id() then
                        p:activate()
                        win:perform_action(actions.CloseCurrentPane { confirm = false }, p)
                    end
                end
            end),
        },
        {
            key = 'z',
            mods = 'LEADER',
            action = actions.TogglePaneZoomState,
        },

        -- Adding in workspace features
        -- Prompt for a name to use for a new workspace and switch to it.
        {
            key = 'w',
            mods = 'LEADER',
            action = actions.PromptInputLine {
                description = wezterm.format {
                    { Attribute = { Intensity = 'Bold' } },
                    { Foreground = { AnsiColor = 'Fuchsia' } },
                    { Text = 'Enter name for new workspace' },
                },
                action = wezterm.action_callback(function(window, pane, line)
                    -- line will be `nil` if they hit escape without entering anything
                    -- An empty string if they just hit enter
                    -- Or the actual line of text they wrote
                    if line then
                        window:perform_action(
                            actions.SwitchToWorkspace {
                                name = line,
                            },
                            pane
                        )
                    end
                end),
            },
        },
        {
            key = 'p',
            mods = 'LEADER',
            action = projects.choose_project(),
        },

        -- Show the launcher in fuzzy selection mode and have it list all workspaces
        -- and allow activating one.
        {
            key = 'o',
            mods = 'LEADER',
            action = actions.ShowLauncherArgs {
                flags = 'FUZZY|WORKSPACES',
            },
        },
    }

    for i = 1, 9 do
        -- LEADER + number to activate that tab
        table.insert(keys, {
            key = tostring(i),
            mods = 'LEADER',
            action = actions.ActivateTab(i - 1),
        })

        if sys.is_linux then
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

    config.leader = { key = 'l', mods = 'CTRL', timeout_milliseconds = 1000 }
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
