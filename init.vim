call plug#begin('~/.local/share/nvim/plugged')
Plug 'djoshea/vim-autoread'
Plug 'hallzy/lightline-onedark'
Plug 'honza/vim-snippets'
Plug 'itchyny/lightline.vim'
Plug 'jiangmiao/auto-pairs'
Plug 'joshdick/onedark.vim'
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --bin' }
Plug 'junegunn/fzf.vim'
Plug 'mhinz/vim-signify'
Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'solarnz/thrift.vim'
Plug 'tmhedberg/SimpylFold'
Plug 'tomlion/vim-solidity'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-speeddating'
Plug 'tpope/vim-surround'
Plug 'wincent/scalpel'
call plug#end()

" leader space
let mapleader = " "

""""""""""""""""""""""""""""""""""
" PLUGIN SPECIFIC

" ctags
set tags=tags

" nerdtree
noremap <c-t> :NERDTreeToggle<cr>
noremap <leader>t :NERDTreeFind<cr>
let g:NERDTreeWinPos = "right"
let g:NERDTreeWinSize = 45
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

" signify
set updatetime=100

" fzf
noremap <c-n> :Buffers<cr>
noremap <c-p> :Files<cr>
let $FZF_DEFAULT_COMMAND = 'ag -g ""'
let g:fzf_buffers_jump = 1
let g:fzf_layout = { 'down': '~35%' }
imap <c-x><c-l> <plug>(fzf-complete-line)
imap <c-x><c-f> <plug>(fzf-complete-path)
nnoremap <leader>a :Ag <c-r>=expand("<cword>")<cr><cr>

" scalpel
let g:ScalpelCommand='Sc'

nnoremap <leader>b :!black %<CR>
nnoremap <leader>u :!~/scripts/update_tags.sh<CR>

"""""""""""""""""""""""""""""""""
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
set title  " Set window title to file
set scrolloff=5  " Never scroll off
set cursorline  " Highlight current line
set clipboard=unnamed  " Copy and paste from system clipboard
set lazyredraw  " Don't redraw while running macros (faster)
set nostartofline  " Vertical movement preserves horizontal position

" Wildmenu completion: use for file exclusions"
set wildmenu
set wildmode=list:longest
set wildignore+=.hg,.git,.svn " Version Controls"
set wildignore+=*.aux,*.out,*.toc "Latex Indermediate files"
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg "Binary Imgs"
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest "Compiled Object files"
set wildignore+=*.spl "Compiled speolling world list"
set wildignore+=*.sw? "Vim swap files"
set wildignore+=*.DS_Store "OSX SHIT"
set wildignore+=*.luac "Lua byte code"
set wildignore+=migrations "Django migrations"
set wildignore+=*.pyc "Python Object codes"
set wildignore+=*.orig "Merge resolution files"
set wildignore+=*.class "java/scala class files"
set wildignore+=*/target/* "sbt target directory"

" No annoying sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

" colors
syntax enable
colorscheme onedark

" indentation
let g:python_recommended_style=0
set tabstop=2 softtabstop=0 expandtab shiftwidth=2 smarttab
if exists('&breakindent') " Indent wrapped lines up to the same level
  set breakindent
endif
set wrap " Visually wrap lines
set linebreak " Intelligently wrap long files

" searching
set nohlsearch
set ignorecase  " Search ignoring case
set smartcase  " Search using smartcase

" cpp header file switch
nnoremap <leader>h :e %:p:s,.h$,.X123X,:s,.cpp$,.h,:s,.X123X$,.cpp,<CR>

" split stuff
set splitbelow
set splitright
set equalalways
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l
nnoremap <c-h> <c-w>h

" folding
set foldlevelstart=99  " start unfolded
nnoremap <s-tab> zA

" http://stackoverflow.com/questions/1551231/highlight-variable-under-cursor-in-vim-like-in-netbeans
:autocmd CursorMoved * exe printf('match IncSearch /\V\<%s\>/', escape(expand('<cword>'), '/\'))
