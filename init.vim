call plug#begin('~/.local/share/nvim/plugged')
Plug 'itchyny/lightline.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'airblade/vim-gitgutter' , { 'on':  'GitGutterEnable' }
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
call plug#end()

""""""""""""""""""""""""""""""""""
" PLUGIN SPECIFIC

" nerdtree
map <f2> :NERDTreeToggle<cr>
let NERDTreeIgnore = ['\.pyc$[[file]]', '\.o$[[file]]']

" lightline
set laststatus=2
let g:lightline = {
  \ 'colorscheme': 'onedark',
  \ }

" gitgutter
set updatetime=250

" fzf
noremap <c-m> :Buffers<cr>
noremap <c-p> :Files<cr>
let $FZF_DEFAULT_COMMAND = 'ag -g ""'
let g:fzf_buffers_jump = 1
let g:fzf_layout = { 'down': '~20%' }
imap <c-x><c-l> <plug>(fzf-complete-line)

""""""""""""""""""""""""""""""""""

" general settings
set nobackup
set noswapfile
set number
set ruler
set mouse=a
set hidden
set formatoptions=croq " Enable comment line auto formatting
set wildignore+=*.o,*.obj,*.class,*.swp,*.pyc " Ignore junk files
set wildmode=longest,list " Better unix-like tab completion
set title " Set window title to file
set scrolloff=5 " Never scroll off
set cursorline " Highlight current line
set clipboard=unnamed " Copy and paste from system clipboard
set lazyredraw " Don't redraw while running macros (faster)
set nostartofline " Vertical movement preserves horizontal position

" leader space
let mapleader = " "

" colors
syntax enable
colorscheme onedark

" indentation
set tabstop=4 softtabstop=0 expandtab shiftwidth=4 autoindent
if exists('&breakindent') " Indent wrapped lines up to the same level
  set breakindent
endif
set wrap " Visually wrap lines
set linebreak " Intelligently wrap long files

" searching
set nohlsearch
set ignorecase  " Search ignoring case
set smartcase  " Search using smartcase

" python formatting
au FileType python setlocal formatprg=autopep8\ -

" split stuff
set splitbelow
set splitright
set equalalways
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l
nnoremap <c-h> <c-w>h
