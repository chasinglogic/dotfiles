-- Begin porting to what would become my init.lua

-- Require settings
require('settings.lsp')
require('settings.tree-sitter')
require('settings.telescope')
require('settings.completion')
require "pears".setup(
    function(conf)
        conf.preset "tag_matching"
    end
)

if vim.fn.has('macunix') then
    vim.g.python3_host_prog = '/usr/local/bin/python3'
end
