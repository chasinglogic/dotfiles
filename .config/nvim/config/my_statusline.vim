function! GitStatusLine(...) abort
  if !exists('b:git_dir')
    return ''
  endif
  return 'git['.FugitiveHead().']'
endfunction

let s:mode_to_english_map = {
      \     'n': 'NORMAL',
      \     'i': 'INSERT',
      \     'R': 'REPLACE',
      \     'v': 'VISUAL',
      \     'V': 'VISUAL',
      \     "\<C-v>": 'VISUAL',
      \     'c': 'COMMAND',
      \     's': 'SELECT',
      \     'S': 'SELECT',
      \     "\<C-s>": 'SELECT',
      \     't': 'TERMINAL'
      \   }

function! StatusLineMode(...)
  return get(s:mode_to_english_map, mode())
endfunction

set statusline=
set statusline+=\ %{StatusLineMode()}
set statusline+=\ %q
set statusline+=\ %f
set statusline+=%=
set statusline+=%{GitStatusLine()}
set statusline+=\ filetype%y
set statusline+=\ L[%l/%L]
set statusline+=\  
