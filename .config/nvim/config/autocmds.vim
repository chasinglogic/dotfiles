""" Terminal stuff
augroup terminal_settings
    autocmd!

    autocmd TermOpen * setlocal nonumber
    autocmd TermOpen * startinsert
    autocmd BufWinEnter,WinEnter term://* startinsert
    autocmd BufLeave term://* stopinsert

    " Ignore fzf as that will close terminal automatically. Otherwise if the
    " shell exits then close the terminal window and buffer.
    autocmd TermClose term://*
          \ if (expand('<afile>') !~ "fzf") |
          \   call nvim_input('<CR>')  |
          \ endif
augroup END

""" Map non-standard files to filetypes
augroup file_type_mappings
  """ Highlight SCons files
  autocmd BufNewFile,BufRead SConscript set filetype=python
  autocmd BufNewFile,BufRead SConstruct set filetype=python
augroup END

""" Vimscript doesn't seem to load ftplugins
augroup folding_in_vimscript
  autocmd BufNewFile,BufRead *.vim set foldmethod=marker
augroup END
