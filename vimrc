set nocompatible

" Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'kien/ctrlp.vim'
Plugin 'scrooloose/nerdcommenter'
Plugin 'bling/vim-airline'
Plugin 'tpope/vim-surround'
Plugin 'altercation/vim-colors-solarized'
Plugin 'airblade/vim-gitgutter'
Plugin 'joshdick/onedark.vim'
Plugin 'ajh17/VimCompletesMe'
Plugin 'junegunn/goyo.vim'
Plugin 'junegunn/limelight.vim'

call vundle#end()            " required
filetype plugin indent on    " required

" leader space
let mapleader=" "

" colors
syntax enable

"set t_Co=256 "256 color
"set background=dark
"let g:solarized_termtrans = 1
"colorscheme solarized

"let g:onedark_termcolors = 256
"colorscheme onedark

colorscheme monokai

" random options
set nobackup
set number
set ruler

" indentation
set backspace=indent,eol,start
set autoindent
"set tabstop=4
"set shiftwidth=4
"set expandtab
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab

" searching
set incsearch

" temp files
set backupdir=~/.vim/backup//
set directory=~/.vim/swap//
set undodir=~/.vim/undo//

" ctrlp stuff
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

" Give a shortcut key to NERD Tree
map <F2> :NERDTreeToggle<CR>
map <F3> :NERDTreeMirror<CR>

" Indent wrapped lines up to the same level
if exists('&breakindent')
  set breakindent
endif

" general settings
set encoding=utf-8 "UTF-8 character encoding
set equalalways  "Split windows equal size
set formatoptions=croq  "Enable comment line auto formatting
set wildignore+=*.o,*.obj,*.class,*.swp,*.pyc "Ignore junk files
set title  "Set window title to file
set ignorecase  "Search ignoring case
set smartcase  "Search using smartcase
set scrolloff=5  "Never scroll off
set wildmode=longest,list  "Better unix-like tab completion
set cursorline  "Highlight current line
set clipboard=unnamed  "Copy and paste from system clipboard
set lazyredraw  "Don't redraw while running macros (faster)
"set autochdir  "Change directory to currently open file
set wrap  "Visually wrap lines
set linebreak  "Only wrap on 'good' characters for wrapping
set backspace=indent,eol,start  "Better backspacing
set linebreak  "Intelligently wrap long files
set ttyfast  "Speed up vim
set nostartofline "Vertical movement preserves horizontal position
set autoread "Reload files

" c0 highlighting
"au BufReadPost *.c0 set syntax=c
"au BufNewFile,BufRead *.c1 set filetype=c

" python formatting
au FileType python setlocal formatprg=autopep8\ -

" airline
set laststatus=2

" easier split navigation
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" gitgutter
set updatetime=250

" mouse
set mouse=a
