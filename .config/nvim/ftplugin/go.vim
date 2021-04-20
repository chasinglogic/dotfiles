if executable('goimports')
    setlocal formatprg='goimports'
endif

augroup go_fmt
    autocmd!
    autocmd BufWritePre *.go Neoformat
augroup END
