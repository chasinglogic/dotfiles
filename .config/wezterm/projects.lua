local wezterm = require('wezterm')
local module = {}

local function file_exists(name)
    local f = io.open(name, "r")
    return f ~= nil and io.close(f)
end

local function project_dirs()
    local projects = { wezterm.home_dir }

    local projector_local_bin = wezterm.home_dir .. "/.local/bin/projector"
    local projector_cargo_bin = wezterm.home_dir .. "/.cargo/bin/projector"
    local projector_bin = projector_local_bin
    if file_exists(projector_cargo_bin) then
        projector_bin = projector_cargo_bin
    end

    wezterm.log_info("Projector bin:" .. projector_bin)

    local success, stdout, _ = wezterm.run_child_process({ projector_bin, 'list' })
    if success then
        for _, line in ipairs(wezterm.split_by_newlines(stdout)) do
            table.insert(projects, line)
        end
    end

    return projects
end

function module.choose_project()
    local choices = {}
    for _, value in ipairs(project_dirs()) do
        table.insert(choices, { label = value })
    end

    return wezterm.action.InputSelector {
        title = "Projects",
        choices = choices,
        fuzzy = true,
        action = wezterm.action_callback(function(child_window, child_pane, id, label)
            -- "label" may be empty if nothing was selected. Don't bother doing anything
            -- when that happens.
            if not label then return end

            -- The SwitchToWorkspace action will switch us to a workspace if it already exists,
            -- otherwise it will create it for us.
            child_window:perform_action(wezterm.action.SwitchToWorkspace {
                -- We'll give our new workspace a nice name, like the last path segment
                -- of the directory we're opening up.
                name = label:match("([^/]+)$"),
                -- Here's the meat. We'll spawn a new terminal with the current working
                -- directory set to the directory that was picked.
                spawn = { cwd = label },
            }, child_pane)
        end),
    }
end

return module
