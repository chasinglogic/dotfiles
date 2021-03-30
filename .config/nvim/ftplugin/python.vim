let s:file_path = expand('%:p')

if match(s:file_path, 'Work') == -1
    set textwidth=88
else
    set textwidth=88
    set makeprg=mpb
endif

if match(s:file_path, 'MPBX') == -1
    """ Autoformat on save.
    augroup autofmt
        autocmd!
        autocmd BufWritePre *.py Neoformat
    augroup END
endif

""" Automatically strip trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e

set tabstop=4
set softtabstop=4
set shiftwidth=4

let b:neoformat_run_all_formatters = 1
