if executable('goimports')
    setlocal formatprg='goimports'
endif

set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()

augroup go_fmt
    autocmd!
    autocmd BufWritePre *.go Neoformat
augroup END
