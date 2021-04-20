set textwidth=88
set makeprg=npm\ run-script\ --silent

" ESLint
" set errorformat+=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#
" TSC
set errorformat=%+A\ %#%f\ %#(%l\\\,%c):\ %m,%C%m

augroup autofmt
    autocmd!
    autocmd BufWritePre *.js,*.jsx,*.ts,*.tsx Neoformat
augroup END
