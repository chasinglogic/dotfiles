vim.g.completion_enable_snippet = 'UltiSnips'

vim.g.completion_chain_complete_list = {
  default = {
    { complete_items = { 'lsp', 'snippet', 'buffers' } },
    { mode = { '<c-p>' } },
    { mode = { '<c-n>' } }
  },
}
