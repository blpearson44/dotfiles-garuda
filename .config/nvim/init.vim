call plug#begin('~/.vim/plugged')
" File tree
Plug 'preservim/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'ryanoasis/vim-devicons'
Plug 'airblade/vim-gitgutter'
" start screen
Plug 'mhinz/vim-startify'
" snippets
Plug 'neoclide/coc.nvim', {'branch': 'release'}
" comments
Plug 'tpope/vim-commentary'
" theme
Plug 'dracula/vim'
Plug 'tribela/vim-transparent'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ap/vim-css-color'
Plug 'luochen1990/rainbow'
" toml syntax highlight
Plug 'cespare/vim-toml'
Plug 'gabrielelana/vim-markdown'
call plug#end()

" Set leader to Space
let mapleader = " "

" Save as root
cmap w!! %!sudo tee > /dev/null %

" theme
colorscheme dracula
let g:rainbow_active = 1 "set to 0 if you want to enable it later via :RainbowToggle

" mappings
nmap <C-n> :NERDTreeToggle<CR>
nmap q :qa<CR>
let NERDTreeMapQuit=''
" nav
map <leader><leader> <C-W>w
map <leader>h <C-W>h
map <leader>j <C-W>j
map <leader>k <C-W>k
map <leader>l <C-W>l
" tab nav
noremap <leader>1 1gt
noremap <leader>2 2gt
noremap <leader>3 3gt
noremap <leader>4 4gt
noremap <leader>5 5gt
noremap <leader>6 6gt
noremap <leader>7 7gt
noremap <leader>8 8gt
noremap <leader>9 9gt
noremap <leader>w :tabclose<CR>


" coc config
let g:coc_global_extensions = [
  \ 'coc-snippets',
  \ 'coc-pairs',
  \ 'coc-tsserver',
  \ 'coc-eslint', 
  \ 'coc-prettier', 
  \ 'coc-json', 
  \ 'coc-toml',
  \ 'coc-python'
  \ ]


" make return auto-select first item in completion
inoremap <silent><expr> <tab> pumvisible() ? coc#_select_confirm()
                              \: "\<C-g>u\<tab>\<c-r>=coc#on_enter()\<CR>"

" run prettier on save
command! -nargs=0 Prettier :CocCommand prettier.formatFile

" NERDTree
let g:NERDTreeGitStatusWithFlags = 1
let g:NERDTreeIgnore = ['^node_modules$']
let g:NERDTreeHighlightCursorline=0
let g:NERDTreeMinimalUI=1
let g:NERDTreeShowHidden=1
" Start NERDTree when Vim is started without file arguments.
" autocmd StdinReadPre * let s:std_in=1
" autocmd VimEnter * if argc() == 0 && !exists('s:std_in') | NERDTree | endif
" Exit Vim if NERDTree is the only window left.
autocmd BufEnter * if tabpagenr('$') == 1 && winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() |
    \ quit | endif
" Open the existing NERDTree on each new tab.
autocmd BufWinEnter * silent NERDTreeMirror

" vim-airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

" from .vimrc
" line numbers
set nonumber
set relativenumber
set nocursorline
set scrolloff=10

" indents
set autoindent
set expandtab
set tabstop=2 shiftwidth=2
set smarttab

" search
set ignorecase
set smartcase
set hlsearch
set incsearch

set showmatch
