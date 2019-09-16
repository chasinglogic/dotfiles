augroup autofmt
  autocmd!
  """ Auto format rust on save
  autocmd BufWritePre *.rs Neoformat
  """ Auto format swift on save
  autocmd BufWritePre *.swift Neoformat
augroup END

""" Terminal stuff
autocmd BufEnter term://* startinsert
autocmd TermOpen * setlocal nonumber
autocmd TermOpen * startinsert

""" Rust CTAGS
autocmd BufRead *.rs :setlocal tags=./rusty-tags.vi;/,$RUST_SRC_PATH/rusty-tags.vi
autocmd BufWritePost *.rs :silent! exec "!rusty-tags vi --quiet --start-dir=" . expand('%:p:h') . "&" | redraw!

" Highlight SCons files
autocmd BufNewFile,BufRead SCons* set syntax=python
