set nocompatible
filetype off

" Vundle
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

Plugin 'VundleVim/Vundle.vim'
Plugin 'scrooloose/nerdtree'
Plugin 'kien/ctrlp.vim'
Plugin 'mxw/vim-jsx'
Plugin 'pangloss/vim-javascript'
Plugin 'scrooloose/nerdcommenter'
Plugin 'unblevable/quick-scope'
Plugin 'bling/vim-airline'
Plugin 'jiangmiao/auto-pairs'
Plugin 'tpope/vim-surround'
Plugin 'digitaltoad/vim-jade'
Plugin 'altercation/vim-colors-solarized'
Plugin 'mattn/emmet-vim'
Plugin 'airblade/vim-gitgutter'
Plugin 'jez/vim-better-sml'

call vundle#end()            " required
filetype plugin indent on    " required

" leader space
let mapleader=" "

" colors
syntax enable
set t_Co=256 "256 color
set background=dark
let g:solarized_termtrans = 1
colorscheme solarized

" random options
set nobackup
set number
set ruler

" indentation
set backspace=indent,eol,start
set autoindent
set tabstop=4
set shiftwidth=4
set expandtab

" searching
nnoremap <leader><space> :noh<return>
set hlsearch
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

" jsx highlighting
let g:jsx_ext_required = 0

" Indent wrapped lines up to the same level
if exists('&breakindent')
  set breakindent
endif

" CMU settings
set encoding=utf-8 "UTF-8 character encoding
"set showmatch  "Highlight matching braces
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
set autochdir  "Change directory to currently open file
set wrap  "Visually wrap lines
set linebreak  "Only wrap on 'good' characters for wrapping
set backspace=indent,eol,start  "Better backspacing
set linebreak  "Intelligently wrap long files
set ttyfast  "Speed up vim
set nostartofline "Vertical movement preserves horizontal position

" c0 highlighting
au BufReadPost *.c0 set syntax=c
au BufNewFile,BufRead *.c1 set filetype=c

" Strip whitespace from end of lines when writing file
autocmd BufWritePre * :%s/\s\+$//e

" for airline
set laststatus=2

" easier splitting
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" youcompleteme stuff
"let g:ycm_global_ycm_extra_conf = '~/.ycm_extra_conf.py'

" gitgutter refresh time
set updatetime=250
