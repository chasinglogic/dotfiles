local modes = {
    ["n"] = "NORMAL",
    ["no"] = "NORMAL",
    ["v"] = "VISUAL",
    ["V"] = "VISUAL LINE",
    [""] = "VISUAL BLOCK",
    ["s"] = "SELECT",
    ["S"] = "SELECT LINE",
    [""] = "SELECT BLOCK",
    ["i"] = "INSERT",
    ["ic"] = "INSERT",
    ["R"] = "REPLACE",
    ["Rv"] = "VISUAL REPLACE",
    ["c"] = "COMMAND",
    ["cv"] = "VIM EX",
    ["ce"] = "EX",
    ["r"] = "PROMPT",
    ["rm"] = "MOAR",
    ["r?"] = "CONFIRM",
    ["!"] = "SHELL",
    ["t"] = "TERMINAL",
}

local function mode()
    local current_mode = vim.api.nvim_get_mode().mode
    return string.format("--%s--", modes[current_mode]):upper()
end

local function lineinfo()
    if vim.bo.filetype == "alpha" then
        return ""
    end

    -- %P - Percentage through the file.
    -- %l - Line Number
    -- %c - Column Number
    return " %P %l:%c "
end

local function filetype()
    return string.format(" ft=%s ", vim.bo.filetype)
end

local function filepath()
    -- :~ - reduces filename to be relative to the home directory, i.e. replace /home/<user> with ~.
    -- :. - reduces filename to be relative to the current directory.
    -- :h - reduces filename to only the head (without this it would be printed twice.)
    local fpath = vim.fn.fnamemodify(vim.fn.expand "%", ":~:.")
    if fpath == "" or fpath == "." then
        return " "
    end

    return string.format(" %%<%s", fpath)
end

Statusline = {}

Statusline.active = function()
    return table.concat {
        "%#Statusline#",
        mode(),
        "%#Normal# ",
        filepath(),
        "%#Normal#",
        "%=%#StatusLineExtra#",
        filetype(),
        lineinfo(),
    }
end

function Statusline.inactive()
    return " %F"
end

function Statusline.short()
    return "%#StatusLineNC#   NvimTree"
end

vim.api.nvim_exec([[
  augroup Statusline
  au!
  au WinEnter,BufEnter * setlocal statusline=%!v:lua.Statusline.active()
  au WinLeave,BufLeave * setlocal statusline=%!v:lua.Statusline.inactive()
  au WinEnter,BufEnter,FileType NvimTree setlocal statusline=%!v:lua.Statusline.short()
  augroup END
]], false)
