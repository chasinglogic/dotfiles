let file_path = expand('%:p')

if match(file_path, 'mongo') == -1
    """ Autoformat on save.
    augroup autofmt
        autocmd!
        autocmd BufWritePre *.py Neoformat
    augroup END
endif
