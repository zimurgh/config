"""""""""""""""""""""""""""""""""""""""""""""
"   File:       .vimrc  Vim Config file     "
"   Author:     Michael Carpenter           "
"   Date:       25/12/12                    "
"""""""""""""""""""""""""""""""""""""""""""""
        
"""""""""""""""""""""""""""""""""""""""""""""
" General
"""""""""""""""""""""""""""""""""""""""""""
call pathogen#infect()
set history=500
filetype plugin on
filetype indent on
set autoread

"""""""""""""""""""""""""""""""""""""""""""""
" Colors and Fonts
"""""""""""""""""""""""""""""""""""""""""""""
syntax enable
let g:solarized_termcolors=256
set background=dark
colorscheme solarized
set encoding=utf8

"""""""""""""""""""""""""""""""""""""""""""""
" Text, Tab, and Indent
"""""""""""""""""""""""""""""""""""""""""""""
set expandtab
set tabstop=4 " number of visual spaces per TAB
set softtabstop=4
set shiftwidth=4
set smarttab
set shiftround
set nojoinspaces
set lbr
"set tw=100

set ai
set si
set wrap

"""""""""""""""""""""""""""""""""""""""""""""
" User Interface
"""""""""""""""""""""""""""""""""""""""""""""
set showcmd             " show command in bottom bar
set so=7
set wildmenu            " visual autocomplete for command menu
set wildmode=full
set wildignore=*.o,*~
set ruler
set cmdheight=1
set number              " show line numbers
set hid
set backspace=eol,start,indent
set whichwrap+=<,>,h,l
set ignorecase
set smartcase
set lazyredraw          " redraw only when we need to
set magic
set showmatch           " highlight matching [{()}]
set mat=2

set noerrorbells
set novisualbell
set t_vb=
set tm=500

"""""""""""""""""""""""""""""""""""""""""""""
" Searching
"""""""""""""""""""""""""""""""""""""""""""""
set incsearch           " search as characters are entered
set hlsearch            " highlight matches

"""""""""""""""""""""""""""""""""""""""""""""
"set foldenable          " enable folding
"set foldlevelstart=5    " how many folds should be opened
"set foldnestmax=10      " 10 nested fold max
"set foldmethod=indent   " fold based on indent level
"""""""""""""""""""""""""""""""""""""""""""""
" Spell Check
"""""""""""""""""""""""""""""""""""""""""""""
autocmd filetype tex setlocal spell spelllang=en_us " spellcheck all .tex files in en_us
autocmd filetype txt setlocal spell spelllang=en_us " spellcheck all .txt files in en_us
augroup MUTT
  au BufRead ~/.mutt/temp/mutt* set spell           " spellcheck email drafts in mutt
augroup END

"""""""""""""""""""""""""""""""""""""""""""""
" Keymaps
"""""""""""""""""""""""""""""""""""""""""""""
map ; :
noremap ;; ;
autocmd filetype tex noremap j gj
autocmd filetype tex noremap k gk
nnoremap <space> za
nnoremap <F5> : set nonumber!<CR>
nnoremap <leader><space> :nohlsearch<CR>
