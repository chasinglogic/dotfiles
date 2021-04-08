-- Begin porting to what would become my init.lua

-- Require settings
require('settings.lsp')
require('settings.tree-sitter')
require('settings.telescope')
require('settings.completion')


if vim.fn.has('macunix') then
    vim.g.python3_host_prog = '/usr/local/bin/python3'
end
