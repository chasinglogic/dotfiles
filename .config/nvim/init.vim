set termguicolors

if has('macunix')
	let g:python3_host_prog='/usr/local/bin/python3'
else
	let g:python3_host_prog='/usr/bin/python3'
endif

source $HOME/.config/nvim/config/editor.vim
source $HOME/.config/nvim/config/keys.vim
source $HOME/.config/nvim/config/plugins.vim
source $HOME/.config/nvim/config/autocmds.vim
source $HOME/.config/nvim/config/utils.vim
source $HOME/.config/nvim/config/my_statusline.vim
source $HOME/.config/nvim/config/abbrevs.vim
lua require('config')

""" Load Theme
set background=dark
let g:gruvbox_contrast_dark='hard'
let g:gruvbox_contrast_light='hard'
colorscheme monokai

highlight! link CmpItemAbbr Pmenu
highlight! link CmpItemKind Pmenu
highlight! link CmpItemMenu Pmenu
