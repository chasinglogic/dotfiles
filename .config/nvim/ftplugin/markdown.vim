let g:markdown_folding = 1

inoremap <buffer> <silent> <Bar>   <Bar><Esc>:call <SID>align()<CR>a

function! s:align()
  let p = '^\s*|\s.*\s|\s*$'
  if exists(':EasyAlign') && getline('.') =~# '^\s*|' && (getline(line('.')-1) =~# p || getline(line('.')+1) =~# p)
    let curpos = getpos(".")
    normal {
    let table_begin = line(".")
    normal }
    normal $
    let table_end = line(".")
    execute table_begin.",".table_end." EasyAlign *|"
    call setpos('.', curpos)
  endif
endfunction
