function! s:findNote(notename)
  let note_dir = ""
  if "$NOTE_DIR" != ""
    let note_dir = $NOTE_DIR
  else
    let note_dir = $HOME/Notes
  endif

  echo note_dir
endfunction

command! -nargs=0 -bar Qargs execute 'args' QuickfixFilenames()
function! QuickfixFilenames()
  " Building a hash ensures we get each buffer only once
  let buffer_numbers = {}
  for quickfix_item in getqflist()
    let buffer_numbers[quickfix_item['bufnr']] = bufname(quickfix_item['bufnr'])
  endfor
  return join(map(values(buffer_numbers), 'fnameescape(v:val)'))
endfunction

""" Hide grep output when running grep
command! -nargs=* Grep :execute ':silent grep "<args>"'
