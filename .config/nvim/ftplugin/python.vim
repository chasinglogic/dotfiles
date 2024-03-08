setlocal textwidth=88
setlocal tabstop=4
setlocal softtabstop=4
setlocal shiftwidth=4

" Autoread must be set for the autoformatting to work. Otherwise the buffer won't
" update after formatting in vim. Alternatively could put `edit` as an autocmd
" but that prompts for enter after save which is annoying.
set autoread
augroup FormatPythonOnSave
    autocmd BufWritePost *.py silent !black --quiet %
    autocmd BufWritePost *.py silent !isort %
augroup END
