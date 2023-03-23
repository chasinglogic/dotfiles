augroup fmt
  autocmd!
  autocmd BufWritePre *.rb lua vim.lsp.buf.format({ async = true })
augroup END
