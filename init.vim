call plug#begin('~/.local/share/nvim/plugged')
Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'tomlion/vim-solidity'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'w0rp/ale'
Plug 'wincent/scalpel'
call plug#end()

""""""""""""""""""""""""""""""""""
" PLUGIN SPECIFIC

" nerdtree
noremap <f2> :NERDTreeToggle<cr>
let NERDTreeIgnore = ['\.pyc$[[file]]', '\.o$[[file]]']

" lightline
set laststatus=2
let g:lightline = {
  \ 'colorscheme': 'onedark',
  \ 'active': {
  \   'left': [ [ 'mode', 'paste' ],
  \             [ 'gitbranch', 'readonly', 'filename', 'modified' ] ],
  \   'right': [ [ 'lineinfo' ],
  \              [ 'percent' ],
  \              [ 'fileformat', 'filetype' ] ]
  \ },
  \ 'inactive': {
  \   'left':  [ [ 'filename', 'modified' ] ],
  \   'right': [ [ 'lineinfo' ],
  \              [ 'percent' ] ]
  \ },
  \ 'component_function': {
  \   'gitbranch': 'fugitive#head'
  \ },
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
imap <c-x><c-f> <plug>(fzf-complete-path)

" scalpel
let g:ScalpelCommand='Sc'

" ALE
let g:ale_linters_explicit = 1
let g:ale_linters = {
  \ 'python': ['flake8'],
  \ }
let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 'never'
nmap <silent> [c <Plug>(ale_previous_wrap)
nmap <silent> ]c <Plug>(ale_next_wrap)
let generic_fixers = ['remove_trailing_lines', 'trim_whitespace']
let g:ale_fixers = {
  \ '*': generic_fixers,
  \ 'python': generic_fixers + ['autopep8', 'isort'],
  \ }
nnoremap <leader>l :ALEFix

""""""""""""""""""""""""""""""""""
" MISC

" general settings
set nobackup
set noswapfile
set number
set relativenumber
set ruler
set mouse=a
set hidden
set formatoptions=croq  " Enable comment line auto formatting
set wildignore+=*.o,*.obj,*.class,*.swp,*.pyc  " Ignore junk files
set wildmode=longest,list  " Better unix-like tab completion
set title  " Set window title to file
set scrolloff=5  " Never scroll off
set cursorline  " Highlight current line
set clipboard=unnamed  " Copy and paste from system clipboard
set lazyredraw  " Don't redraw while running macros (faster)
set nostartofline  " Vertical movement preserves horizontal position

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

" folding
if has('folding')
  if has('windows')
    let &fillchars='vert: '  " less cluttered vertical window separators
  endif
  set foldmethod=indent  " not as cool as syntax, but faster
  set foldlevelstart=99  " start unfolded
endif
nnoremap <s-tab> zA

" leader maps
nnoremap <leader>r :!./rsync.sh
