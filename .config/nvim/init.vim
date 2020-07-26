let g:python3_host_prog="/usr/bin/python3"

let $NVIM_TUI_ENABLE_TRUE_COLOR=1
filetype plugin on
syntax enable
set termguicolors

""" Theme settings
let g:base16colorspace=256
let g:neosolarized_contrast = "high"

source $HOME/.config/nvim/config/editor.vim
source $HOME/.config/nvim/config/keys.vim
if !exists('g:vscode')
    source $HOME/.config/nvim/config/plugins.vim
    source $HOME/.config/nvim/config/autocmds.vim
    source $HOME/.config/nvim/config/utils.vim
    source $HOME/.config/nvim/config/my_statusline.vim
    source $HOME/.config/nvim/config/abbrevs.vim
endif

""" Load Theme
let s:current_hour = str2nr(strftime("%H"))
if !empty("$VIM_BG")
  set background="$VIM_BG"
elseif 7 < s:current_hour && s:current_hour < 18 
  set background=light
else
  set background=dark
endif

colorscheme NeoSolarized

""" Set font for GUI
if has('gui_running')
    set guifont=Hack:h16
end
