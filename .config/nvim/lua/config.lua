-- Begin porting to what would become my init.lua

-- Require settings
require('settings.lsp')
require('settings.tree-sitter')
require('settings.telescope')
require('settings.completion')
require('settings.autotag')
require('settings.autopairs')

-- Configure lua themes
require('github-theme').setup({
    hide_inactive_statusline = false,
    comment_style = 'NONE',
})
