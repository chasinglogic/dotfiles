set textwidth=88
setlocal makeprg=npm\ run-script\ --silent

set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()

" ESLint
" set errorformat+=%f:\ line\ %l\\,\ col\ %c\\,\ %m,%-G%.%#
" TSC
set errorformat=%+A\ %#%f\ %#(%l\\\,%c):\ %m,%C%m

setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4

augroup autofmt
    autocmd!
    autocmd BufWritePre *.js,*.jsx,*.ts,*.tsx Neoformat
augroup END
