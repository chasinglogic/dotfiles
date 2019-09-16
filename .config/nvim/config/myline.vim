let g:git_branch_checked = 0
let g:git_branch = ""

function! GitBranch()
  return system("git rev-parse --abbrev-ref HEAD 2>/dev/null | tr -d '\n'")
endfunction

function! StatuslineGit()
  let l:branchname = ''
  if g:git_branch_checked
    let l:branchname = g:git_branch
  else
    let l:branchname = GitBranch()
    let g:git_branch_checked = 1
    let g:git_branch = l:branchname
  endif

  return strlen(l:branchname) > 0?'  '.l:branchname.' ':''
endfunction

let g:last_task_check = []
let g:last_task = ''
function! GetCurrentTask()
    let g:last_task = system("task next --title-only | tr -d '\n'")
    let g:last_task_check = reltime()
    return g:last_task
endfunction

function! CurrentTask()
  if len(g:last_task_check) != 0
    if reltimefloat(reltime(g:last_task_check)) > 120.0
      return GetCurrentTask()
    else
      return g:last_task
    endif
  else
    return GetCurrentTask()
  endif
endfunction

set statusline=
set statusline+=%{StatuslineGit()}
set statusline+=\ %t
set statusline+=%=
set statusline+=\ %y
set statusline+=\ L:%l/%L
set statusline+=\  
