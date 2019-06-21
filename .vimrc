set rtp+=~/.fzf
set nocompatible              " required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle, required
Plugin 'gmarik/Vundle.vim'

" add all your plugins here (note older versions of Vundle
" used Bundle instead of Plugin)

Plugin 'vim-scripts/indentpython.vim'
Bundle 'Valloric/YouCompleteMe'
Plugin 'vim-syntastic/syntastic'
Plugin 'nvie/vim-flake8'
Plugin 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
Plugin 'connorholyday/vim-snazzy'
Plugin 'w0rp/ale'
Plugin 'elixir-editors/vim-elixir'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'

" All of your Plugins must be added before the following line
call vundle#end()            " required
filetype plugin indent on    " required

colorscheme snazzy
let g:airline_theme='solarized'
let g:airline_powerline_fonts = 1

set number
set colorcolumn=80
set ruler
set cursorline

" filetype plugin on
let python_highlight_all=1
" elixir stuff
"let g:ale_elixir_elixir_ls_release = '~/.config/vimpackages/elixir-ls/rel'
syntax on
