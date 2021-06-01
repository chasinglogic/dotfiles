let s:file_path = expand('%:p')

if match(s:file_path, 'Work') == -1
    set textwidth=88
else
    set textwidth=88
    set makeprg=mpb
endif

setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4

augroup fmt
  autocmd!
  autocmd BufWritePre *.py Neoformat
augroup END

let b:neoformat_run_all_formatters = 1
