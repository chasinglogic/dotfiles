augroup autofmt
    autocmd!
    autocmd BufWritePre *.js,*.jsx Neoformat
augroup END

set makeprg="npm run-script"
