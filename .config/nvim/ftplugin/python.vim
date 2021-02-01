let s:file_path = expand('%:p')

set tabstop=4
set softtabstop=4
set shiftwidth=4

set shiftwidth=4

if match(s:file_path, 'Work') == -1
    set textwidth=120
else
    set textwidth=90
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
