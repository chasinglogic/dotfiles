""" Rust CTAGS
autocmd BufRead *.rs :setlocal tags=./rusty-tags.vi;/,$RUST_SRC_PATH/rusty-tags.vi
autocmd BufWritePost *.rs :silent! exec "!rusty-tags vi --quiet --start-dir=" . expand('%:p:h') . "&" | redraw!

""" Auto format rust on save
autocmd BufWritePre *.rs Neoformat

set foldmethod=expr
set foldexpr=nvim_treesitter#foldexpr()
