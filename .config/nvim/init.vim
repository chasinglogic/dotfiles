let $NVIM_TUI_ENABLE_TRUE_COLOR=1
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
lua require('config')

""" Load Theme
set background=light

" Default value is "normal", Setting this option to "high" or "low" does use the
" same Solarized palette but simply shifts some values up or down in order to
" expand or compress the tonal range displayed.
let g:neosolarized_contrast = "high"

let g:github_comment_style = 'NONE'

colorscheme github_light

highlight! link CmpItemAbbr Pmenu
highlight! link CmpItemKind Pmenu
highlight! link CmpItemMenu Pmenu
