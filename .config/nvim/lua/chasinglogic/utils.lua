local M = {}

local system

if string.match(vim.loop.os_uname().release, "WSL") then
    system = "WSL"
else
    system = vim.loop.os_uname().sysname
end

if system == "Darwin" then
    M.query_command = "defaults read -g AppleInterfaceStyle"
elseif system == "Linux" then
    if not vim.fn.executable("dbus-send") then
        error([[
		`dbus-send` is not available. The Linux implementation of
		auto-dark-mode.nvim relies on `dbus-send` being on the `$PATH`.
	  ]])
    end

    M.query_command = table.concat({
        "dbus-send --session --print-reply=literal --reply-timeout=1000",
        "--dest=org.freedesktop.portal.Desktop",
        "/org/freedesktop/portal/desktop",
        "org.freedesktop.portal.Settings.Read",
        "string:'org.freedesktop.appearance'",
        "string:'color-scheme'",
    }, " ")
elseif system == "Windows_NT" or system == "WSL" then
    -- Don't swap the quotes; it breaks the code
    M.query_command =
    'reg.exe Query "HKCU\\SOFTWARE\\Microsoft\\Windows\\CurrentVersion\\Themes\\Personalize" /v AppsUseLightTheme | findstr.exe "AppsUseLightTheme"'
end

local function parse_query_response(res)
    if system == "Linux" then
        -- https://github.com/flatpak/xdg-desktop-portal/blob/c0f0eb103effdcf3701a1bf53f12fe953fbf0b75/data/org.freedesktop.impl.portal.Settings.xml#L32-L46
        -- 0: no preference
        -- 1: dark
        -- 2: light
        return string.match(res, "uint32 1") ~= nil
    elseif system == "Darwin" then
        return res == "Dark"
    elseif system == "Windows_NT" or system == "WSL" then
        -- AppsUseLightTheme    REG_DWORD    0x0 : dark
        -- AppsUseLightTheme    REG_DWORD    0x1 : light
        return string.match(res, "1") == nil
    end
    return false
end

M.set_theme_according_to_os_dark_mode = function(light_theme, dark_theme)
    vim.fn.jobstart(M.query_command, {
        stdout_buffered = true,
        on_stdout = function(_, data, _)
            -- we only care about the first line of the response
            local is_dark_mode = parse_query_response(data[1])
            local theme_to_use = light_theme
            if is_dark_mode then
                theme_to_use = dark_theme
            end

            if theme_to_use ~= vim.g.colors_name then
                vim.cmd.colorscheme(theme_to_use)
            end
        end,
    })
end

M.keep_theme_in_sync_with_os_dark_mode = function(light_theme, dark_theme)
    local timer = vim.uv.new_timer()
    local interval = 1000
    timer:start(interval, interval, function()
        vim.schedule(function()
            M.set_theme_according_to_os_dark_mode(light_theme, dark_theme)
        end)
    end)
end

return M
