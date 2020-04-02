let $NVIM_TUI_ENABLE_TRUE_COLOR=1
let g:python3_host_prog = '/usr/bin/python3'
let g:python_host_prog = '/usr/bin/python3'
filetype plugin on
syntax enable
set termguicolors
set background=light

""" Theme settings
let g:base16colorspace=256
let g:neosolarized_contrast = "high"

source $HOME/.config/nvim/config/editor.vim
source $HOME/.config/nvim/config/plugins.vim
source $HOME/.config/nvim/config/keys.vim
source $HOME/.config/nvim/config/autocmds.vim
source $HOME/.config/nvim/config/utils.vim
source $HOME/.config/nvim/config/my_statusline.vim

""" Load Theme
colorscheme NeoSolarized

""" Set font for GUI
if has('gui_running')
    set guifont=Hack:h16
end

set shortmess=O
