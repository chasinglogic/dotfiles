function! s:findNote(notename)
  let note_dir = ""
  if "$NOTE_DIR" != ""
    let note_dir = $NOTE_DIR
  else
    let note_dir = $HOME/Notes
  endif

  echo note_dir
endfunction

""" Hide grep output when running grep
command! -nargs=* Grep :execute ':silent grep "<args>"'
