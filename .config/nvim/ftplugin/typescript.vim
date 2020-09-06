set textwidth=80
set makeprg="npm run-script"
augroup autofmt
    autocmd!
    autocmd BufWritePre *.ts,*.tsx Neoformat
augroup END
