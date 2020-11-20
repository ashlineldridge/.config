set nocompatible
filetype off

" Use the Vundle package manager. All plugins are declared
" between the Vundle begin() and end() calls.
set rtp+=~/.config/nvim/bundle/Vundle.vim
call vundle#begin()

Plugin 'gmarik/Vundle.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'scrooloose/nerdcommenter'
Plugin 'chriskempson/base16-vim'
Plugin 'maksimr/vim-jsbeautify'
Plugin 'editorconfig/editorconfig-vim'

call vundle#end()
filetype plugin indent on

" The almighty leader keys
let mapleader=";"

" Set up the Base16 Tomorrow Dark theme
let base16colorspace=256
colorscheme base16-tomorrow
set background=dark

" Other codey settings
syntax on
" set ts=4
set backspace=indent,eol,start

" Enable Scala syntax highlighting for .sc files
au BufReadPost *.sc set syntax=scala

" Search settings
set incsearch hlsearch

" NERDTree configuration and key mappings
map <C-n> :NERDTreeToggle<cr>
map <leader>nf :NERDTreeFind<cr>
" Allow quit command to exit Vim if the NERDTree window is the last open
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTreeType") && b:NERDTreeType == "primary") | q | endif
"let NERDTreeShowHidden=1

" JSON Beautification
map <leader>j :call JsBeautify()<cr>
vmap <buffer> <leader>j :call RangeJsBeautify()<cr>
"autocmd FileType json noremap <buffer> <leader>j :call JsonBeautify()<cr>
"autocmd FileType json vnoremap <buffer>  <leader>j :call RangeJsBeautify()<cr>
