let g:go_fmt_command = "goimports"

" IDE-esque commands
nmap <buffer> <Leader>rr :GoRename 

" Go specific commands
nmap <buffer> <Leader>ot :GoAddTags<CR>
nmap <buffer> <Leader>oi :GoImpl 
nmap <buffer> <Leader>od <Plug>(go-doc-browser)

" Common keys rebound specific to Go
nmap <buffer> <Leader>gt :GoAlternate<CR>
nmap <buffer> <Leader>gd <Plug>(go-def)
