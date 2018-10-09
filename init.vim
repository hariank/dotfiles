call plug#begin('~/.local/share/nvim/plugged')
Plug 'itchyny/lightline.vim'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-commentary'
Plug 'yggdroot/indentline'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' } " load only on toggle
call plug#end()

" leader space
let mapleader=" "

" colors
syntax enable
colorscheme onedark

" indentation
set tabstop=8 softtabstop=0 expandtab shiftwidth=4
if exists('&breakindent') " Indent wrapped lines up to the same level
  set breakindent 
endif

" searching
set nohlsearch
set ignorecase  " Search ignoring case
set smartcase  " Search using smartcase

" general settings
set nobackup
set number
set ruler
set mouse=a " mouse
set equalalways  " Split windows equal size
set formatoptions=croq  " Enable comment line auto formatting
set wildignore+=*.o,*.obj,*.class,*.swp,*.pyc " Ignore junk files
set title  " Set window title to file
set scrolloff=5 " Never scroll off
set wildmode=longest,list " Better unix-like tab completion
set cursorline " Highlight current line
set clipboard=unnamed  " Copy and paste from system clipboard
set lazyredraw  " Don't redraw while running macros (faster)
set wrap  " Visually wrap lines
set linebreak  " Intelligently wrap long files
set nostartofline " Vertical movement preserves horizontal position

" python formatting
au FileType python setlocal formatprg=autopep8\ -

" split stuff
set splitbelow
set splitright

" nerdtree
map <f2> :nerdtreetoggle<cr>
map <f3> :nerdtreemirror<cr>

" airline/lightline
set laststatus=2
let g:lightline = {
  \ 'colorscheme': 'onedark',
  \ }

" gitgutter
set updatetime=250

" indentline
let g:indentline_char = '┊'
