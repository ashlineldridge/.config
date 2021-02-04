" This init file expects vim-plug to be installed. See
" instructions here: https://github.com/junegunn/vim-plug.

call plug#begin('~/.cache/nvim/plugins')

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

call plug#end()

map <C-n> :NERDTreeToggle<cr>
