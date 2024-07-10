local M = {}
local wezterm = require 'wezterm'

M.is_os = function(name)
    return wezterm.target_triple:find(name) ~= nil
end

M.is_linux = M.is_os("linux")
M.is_macos = M.is_os("darwin")

return M
