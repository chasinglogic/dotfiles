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

function! CopyRelativePath()
  let l:relpath = expand("%")
  let @+=l:relpath
  return l:relpath
endfunction
command! CopyRelativePath :call CopyRelativePath()

function! RunTest()
  let l:fp = CopyRelativePath()
  set makeprg=./common/scripts/tests
  let @+=b:makeprg . " " . l:fp
  execute ":make " . l:fp
endfunction
command! RunTest :call RunTest()
