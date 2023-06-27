require'nvim-treesitter.configs'.setup {
    auto_install = true,
    ensure_installed = {
      "bash",
      "c",
      "cpp",
      "css",
      "dockerfile",
      "eex",
      "elixir",
      "go",
      "heex",
      "html",
      "javascript",
      "jsdoc",
      "json",
      "jsonc",
      "lua",
      "php",
      "python",
      "r",
      "ruby",
      "rust",
      "swift",
      "toml",
      "typescript",
      "vala",
      "vim",
      "vue",
      "yaml"
  },

  highlight = {
    enable = true,
    disable = {"embedded_template"},
  },

  autotag = {
    enable = true,
  }
}
