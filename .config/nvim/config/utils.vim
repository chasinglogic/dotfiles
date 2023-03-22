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
  execute ":make test " . l:fp
endfunction
command! RunTest :call RunTest()

let g:ScratchBufferName = "__scratch__"
function! OpenScratchBuffer()
    " Check whether the scratch buffer is already created
    let scr_bufnum = bufnr(g:ScratchBufferName)

    " open a new scratch buffer
    if scr_bufnum == -1
        execute "edit " . g:ScratchBufferName
        call s:ScratchMarkBuffer()
    else
        " Scratch buffer is already created. Check whether it is open
        " in one of the windows
        let scr_winnum = bufwinnr(scr_bufnum)
        if scr_winnum != -1
            " Jump to the window which has the scratch buffer if we are not
            " already in that window
            if winnr() != scr_winnum
                exe scr_winnum . "wincmd w"
            endif
        else
            exe "buffer " . scr_bufnum
        endif
    endif
endfunction

" ScratchMarkBuffer
" Mark a buffer as scratch
function! s:ScratchMarkBuffer()
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    setlocal buflisted
endfunction

autocmd BufNewFile g:ScratchBufferName call s:ScratchMarkBuffer()
command! SC :call OpenScratchBuffer()
