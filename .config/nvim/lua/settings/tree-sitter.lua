local install = require'nvim-treesitter.install'
install.ensure_installed('maintained')

require'nvim-treesitter.configs'.setup {
  highlight = {
    enable = true,
    disable = {"yaml", "vim"},
  }
}
