vim.o.autoindent = true
vim.o.number = true
vim.o.wrap = false
vim.o.signcolumn = "yes"

vim.g.mapleader = " "
vim.diagnostic.config({ virtual_text = true })

vim.keymap.set('i', 'fd', '<ESC>')

vim.keymap.set('n', '<leader>fs', ':w<CR>')
vim.keymap.set('n', '<leader>fq', ':wq<CR>')

vim.keymap.set('n', '<leader>w', '<C-w>')
vim.keymap.set('n', '<leader>r', ':source ~/.config/nvim/init.lua<CR>')

vim.pack.add({
  { src = "https://github.com/catppuccin/nvim" },
  { src = "https://github.com/neovim/nvim-lspconfig" },
  { src = "https://github.com/mason-org/mason.nvim" },
  { src = "https://github.com/mason-org/mason-lspconfig.nvim" },
  { src = "https://github.com/WhoIsSethDaniel/mason-tool-installer.nvim" },
  { src = "https://github.com/nvim-mini/mini.nvim", version = 'main' },
  { src = "https://github.com/nvim-treesitter/nvim-treesitter", version = 'main' },
})

require('mini.pairs').setup()
require('mini.surround').setup()
require('mini.splitjoin').setup()
require('mini.comment').setup()
require('mini.snippets').setup()
require('mini.icons').setup()
require('mini.completion').setup()

require("mason").setup()
require("mason-lspconfig").setup()
require("mason-tool-installer").setup({
  ensure_installed = {"lua_ls"}
})

vim.cmd("colorscheme catppuccin-mocha")
