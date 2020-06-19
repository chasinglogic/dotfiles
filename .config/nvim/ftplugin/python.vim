let s:file_path = expand('%:p')

if match(s:file_path, 'Work') == -1
    """ Autoformat on save.
    augroup autofmt
        autocmd!
        autocmd BufWritePre *.py Neoformat
    augroup END

    set textwidth=100
    let test#python#runner = 'pytest'
else
    if match(s:file_path, 'MPBX') != -1
        let test#python#runner = 'djangotest'
        let test#python#djangotest#executable = 'cd mpb && python manage.py test'
        let test#python#nosettests#executable = 'cd mpb && python manage.py test'
    else
        let test#python#runner = 'pytest'
        let test#python#pytest#executable = './common/scripts/test'
    endif
endif

if match(s:file_path, 'SConstruct') != -1 || match(s:file_path, 'SConscript') != -1
    let b:ale_linters = []
endif

""" Automatically strip trailing whitespace on save
autocmd BufWritePre * %s/\s\+$//e
