imap fd <ESC>

let mapleader = " "

nmap <Tab> ==

nmap s S
vmap s S

nmap <Leader><Leader> :Commands<CR>

" Clear highlights
nmap <Leader>noh :noh<CR>

""" Files
nmap <Leader>fs :w<CR>
nmap <Leader>ff :FZF<CR>
nmap <Leader>fe :find
nmap <Leader>fd :find ~/.config/nvim/init.vim<CR>
nmap <Leader>fD :Delete<CR>
nmap <Leader>fr :Rename
nmap <Leader>fR :source %<CR>

""" Project level ops
nmap <Leader>pf :FZF<CR>
nmap <Leader>ps :RG<CR>
nmap <Leader>pt :Tags<CR>
nmap <Leader>pg :Grep<Space>
nmap <Leader>pq :FZFQuickFix<CR>
nmap <Leader>pa :args `git ls-files`<CR>:argdo
nmap <Leader>pr :args `git ls-files`<CR>:argdo %s/
nmap <Leader>pc :make<CR>

""" Lists
nmap <Leader>en :lnext<CR>
nmap <Leader>ep :lprev<CR>
nmap <Leader>qn :cnext<CR>
nmap <Leader>qn :cprev<CR>

nmap ]q :cnext<CR>
nmap [q :cprev<CR>
nmap ]wq :cwindow<CR>
nmap ]wl :lwindow<CR>
nmap ]l :lnext<CR>
nmap [l :lprev<CR>

""" Jumps
nmap <Leader>j= gg=G<C-o>:echo "Indented buffer"<CR>
nmap <Leader>jp <C-o>
nmap <Leader>jn <C-i>
nmap <Leader>ji :FZFTag<CR>
nmap <Leader>jtd :ALEGotoDefitinion<CR>

""" Buffers
nmap <Leader>bb :Buffers<CR>
nmap <Leader>bd :bdelete<CR>
nmap <Leader>bD :bdelete!<CR>
nmap <Leader>bs :tabnew __scratch__<CR>:setlocal buftype=nofile<CR>:setlocal bufhidden=hide<CR>:setlocal noswapfile<CR>
nmap <Leader>br :e %<CR>

""" Windows
nmap <Leader>w <C-w>
nmap <C-w>d <C-w>c
nmap <C-w>m <C-w>o

""" Terminal
nmap <Leader>'  :split term://zsh<CR>
nmap <Leader>xx :terminal<CR>
nmap <Leader>xt :tabnew<CR>:terminal<CR>
tnoremap fd <C-\><C-n>
tnoremap jk <C-\><C-n>:tabprev<CR>

""" Tabs
nmap <Leader>tn :tabnext<CR>
nmap <Leader>tp :tabprev<CR>
nmap <Leader>to :tabnew<CR>
nmap <Leader>tc :tabclose<CR>
nmap <Leader>tw <C-w>T
nmap <Leader>t1 :tabn 1<CR>
nmap <Leader>t2 :tabn 2<CR>
nmap <Leader>t3 :tabn 3<CR>
nmap <Leader>t4 :tabn 4<CR>
nmap <Leader>t5 :tabn 5<CR>

""" Git
nmap <Leader>ga :!git add %<CR>
nmap <Leader>gc :Gcommit<CR>
nmap <Leader>gs :Gstatus<CR>
nmap <Leader>gb :Gblame<CR>

""" Utility
nmap YY ggyG<C-o><C-o> " Copy whole buffer without losing my place

""" System / Vim management
nmap <Leader>spi :PlugInstall<CR>
nmap <Leader>spu :PlugUpdate<CR>
nmap <Leader>spg :PlugUpgrade<CR>
nmap <Leader>sps :PlugStatus<CR>
nmap <Leader>spc :PlugClean<CR>
nmap <Leader>sq  :q<CR>
nmap <Leader>q  :q<CR>

""" Writing
nmap <Leader>rm :Goyo<CR>
nmap <Leader>rww :Wordy
nmap <Leader>rwn :NextWordy<CR>
nmap <Leader>rwp :PrevWordy<CR>
nmap <Leader>rwo :NoWordy<CR>

""" Linting
nmap [a <Plug>(ale_previous_wrap)
nmap ]a <Plug>(ale_next_wrap)

""" Building
nmap <Leader>m :make
