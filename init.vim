call plug#begin('~/.local/share/nvim/plugged')
Plug 'scrooloose/nerdtree'
Plug 'bling/vim-airline'
Plug 'tpope/vim-surround'
Plug 'airblade/vim-gitgutter'
Plug 'joshdick/onedark.vim'
Plug 'junegunn/goyo.vim'
Plug 'junegunn/limelight.vim'
Plug 'tpope/vim-commentary'
call plug#end()

" leader space
let mapleader=" "

" colors
syntax enable
let g:onedark_termcolors = 256
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

" easier split navigation
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" NERDTree
map <F2> :NERDTreeToggle<CR>
map <F3> :NERDTreeMirror<CR>

" airline
set laststatus=2

" gitgutter
set updatetime=250

