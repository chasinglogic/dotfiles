local M = {}
local Job = require('plenary.job')

M.timer = vim.loop.new_timer()

function M.theme_mode()
    local output = io.popen('defaults read -g AppleInterfaceStyle'):read()
    if output == "Dark" then
        return 'dark'
    else
        return 'light'
    end
end


function M.switch_theme()
    vim.o.background = M.theme_mode()
end


-- Check every half hour (apparently nvim_set_option cannot be called in a lua
-- callback which is what is happening here. The docs are very sparse on this so
-- will have to find an alternate solution.)
-- M.check_interval = 1000 * 60 * 30
-- M.timer:start(M.check_interval, 0, M.switch_theme)

return M
