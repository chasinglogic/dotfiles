require'nvim-treesitter.configs'.setup {
  ensure_installed = { 
      "bash", 
      "c", 
      "cpp",
      "dockerfile",
      "go",
      "html",
      "javascript",
      "jsdoc",
      "json",
      "jsonc",
      "lua", 
      "python",
      "r",
      "ruby",
      "rust", 
      "swift",
      "typescript",
      "toml",
      "vala",
      "vim",
      "yaml",
      "vue"
  },

  highlight = {
    enable = true,
    -- disable = {"yaml", "vim"},
  },

  autotag = {
    enable = true,
  }
}
