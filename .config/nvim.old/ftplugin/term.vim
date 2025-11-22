""" Terminal stuff
augroup terminal_settings
    autocmd!

    autocmd TermOpen * setlocal nonumber
    autocmd BufWinEnter,WinEnter term://* startinsert
    autocmd BufLeave term://* stopinsert

    " Ignore fzf as that will close terminal automatically. Otherwise if the
    " shell exits then close the terminal window and buffer.
    autocmd TermClose term://*
          \ if (expand('<afile>') !~ "fzf") |
          \   call nvim_input('<CR>')  |
          \ endif
augroup END
