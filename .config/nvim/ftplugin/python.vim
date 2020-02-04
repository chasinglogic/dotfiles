let file_path = expand('%:p')

if match(file_path, 'mongo') == -1
    """ Autoformat on save.
    autocmd BufWritePre *.py Neoformat
endif
