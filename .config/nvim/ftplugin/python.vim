let file_path = expand('%:p')

if match(file_path, 'Work') == -1
    """ Autoformat on save.
    augroup autofmt
        autocmd!
        autocmd BufWritePre *.py Neoformat
    augroup END
endif

if match(file_path, 'SConstruct') != -1 || match(file_path, 'SConscript') != -1
    let b:ale_linters = []
endif

""" Automatically strip trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e
