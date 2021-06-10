-- Begin porting to what would become my init.lua

-- Require settings
require('settings.lsp')
require('settings.tree-sitter')
require('settings.telescope')
require('settings.completion')
require "pears".setup(
    function(conf)
        -- TODO: re-enable this thing
        -- conf.preset "tags"
        conf.disabled_filetypes {"gitcommit", "fugitive", "telescope", "prompt"}
    end
)

if vim.fn.has('macunix') then
    vim.g.python3_host_prog = '/usr/local/bin/python3'
end
