" Leader setup. {{{
let mapleader = " "
" }}}
" Files {{{
nmap <Leader>fs :w<CR>
nmap <Leader>fq :wq<CR>
" }}}
" Project level ops {{{
nmap <Leader>pf :Telescope find_files previewer=false hidden=true<CR>
nmap <Leader>ps :Telescope live_grep<CR>
nmap <Leader>pS :lua require("telescope").extensions.live_grep_args.live_grep_args()<CR>
nmap <Leader>pc :make
nmap <Leader>h :lua require("replacer").run({ rename_files = false })<cr>
nmap <Leader>H :lua require("replacer").run({ rename_files = true })<cr>
nmap <Leader>pa :args `rg --files --hidden --ignore-vcs -g '!{**/node_modules/*,**/.git/*}'`<cr>
nmap <Leader>pg :grep  
" }}}
" Lists {{{
nmap ]q  :cnext<CR>
nnoremap [q  :cprev<CR>
nmap ]wq :cwindow<CR>
nmap ]wl :lwindow<CR>
nmap ]l  :lnext<CR>
nmap [l  :lprev<CR>
" }}}
" Jumps {{{
nmap <Leader>j=  gg=G<C-o>:echo "Indented buffer"<CR>
nmap <Leader>jp  <C-o>
nmap <Leader>jn  <C-i>
" }}}
" Buffers {{{
nmap <Leader>bb :Telescope buffers<CR>
" Switch to the last buffer and delete this one.
nmap <Leader>bd :bprevious\|bdelete #<CR>
nmap <Leader>bs :SC<CR>
nmap <Leader>br :e %<CR>
nmap <Leader>bf :Neoformat<CR>
" }}}
" Windows {{{
nmap <Leader>w <C-w>
nmap <C-w>m <C-w>o
map <M-o> <C-w>w
" }}}
" Terminal {{{
nmap <Leader>'  :terminal<CR>
tnoremap fd     <C-\><C-n>
tmap <C-o> <C-\><C-n>
nmap <Leader>1 :!
" }}}
" Tabs {{{
nmap ]t :tabnext<CR>
nmap [t :tabprev<CR>
tnoremap <M-j> <C-\><C-n>:tabnext<CR>
tnoremap <M-k> <C-\><C-n>:tabprev<CR>
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
" }}}
" Git {{{
nmap <Leader>gg  :Git
nmap <Leader>gs  :Git<CR>
nmap <Leader>gl  :Git log<CR>
nmap <Leader>gp  :Git push<CR>
nmap <Leader>gP  :Git pull --rebase<CR>
nmap <Leader>gri :Git rebase -i origin/master
nmap <Leader>gb  :Git blame<CR>
nmap <Leader>gv  :Git vader<CR>
" }}}
" Utility {{{
imap fd <ESC>
" Copy file name to clipboard
nmap ,cs :let @*=expand("%")<CR>
nmap ,cl :let @*=expand("%:p")<CR>
" On MacOS I tell my keyboard to use alt as meta as the Lord intended but that
" means I can't type the hash symbol on my UK keyboards. This fixes that.
if has("macunix")
    imap <M-3> #
endif
" Easily run commands
nmap ! :!
" }}}
" System / Vim management {{{
nmap <Leader>spi :PlugInstall<CR>
nmap <Leader>spu :PlugUpdate<CR>
nmap <Leader>spg :PlugUpgrade<CR>
" }}}
" Linting {{{
nmap [a <Plug>(ale_previous_wrap)
nmap ]a <Plug>(ale_next_wrap)
" }}}
" Easy alignment {{{
vmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)
" }}}
" Testing {{{
noremap <Leader>mt :RunTest<CR>
" }}}
