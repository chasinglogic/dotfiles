set textwidth=88
setlocal makeprg=npm\ run-script\ --silent

" ESLint
" set errorformat+=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#
" TSC
set errorformat=%+A\ %#%f\ %#(%l\\\,%c):\ %m,%C%m

setlocal tabstop=2
setlocal softtabstop=2
setlocal shiftwidth=2

augroup autofmt
    autocmd!
    autocmd BufWritePre *.js,*.jsx,*.ts,*.tsx Neoformat
augroup END
