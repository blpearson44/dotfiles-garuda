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
" Messes with Neovide
" Plug 'tribela/vim-transparent'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ap/vim-css-color'
Plug 'luochen1990/rainbow'
" toml syntax highlight
Plug 'cespare/vim-toml'
Plug 'gabrielelana/vim-markdown'
" LaTeX
Plug 'lervag/vimtex'
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
noremap <leader>tc :tabclose<CR>
" Splits
noremap <leader>wc <C-W>q
noremap <leader>wv <C-W>v
noremap <leader>wr <C-W>r
noremap <leader>wt <C-W>T
noremap <leader>w= <C-W>=

" error nav
nmap <silent> [e <Plug>(coc-diagnostic-prev)
nmap <silent> e] <Plug>(coc-diagnostic-next)

" coc config
let g:coc_global_extensions = [
  \ 'coc-snippets',
  \ 'coc-pairs',
  \ 'coc-tsserver',
  \ 'coc-eslint', 
  \ 'coc-prettier', 
  \ 'coc-json', 
  \ 'coc-toml',
  \ 'coc-python',
  \ 'coc-texlab'
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

" Startify
let g:startify_bookmarks = ['~/.config/nvim/init.vim', '~/.config/fish/config.fish', '~/.config/qtile/config.py', '~/Projects/']
let g:startify_session_dir = '~/.config/nvim/sessions'
let g:startify_session_autload = 0
let g:startify_session_persistence = 1
let g:startify_lists =[
      \ { 'type': 'bookmarks',  'header': ['  Bookmarks']   },
      \ { 'type': 'sessions',   'header': ['  Sessions']    },
      \ { 'type': 'dir',      'header': ['  MRU '.getcwd()] },
      \ ] 

" vim-airline
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tab_nr_type = 1


" vimtex
let g:vimtex_view_method = 'zathura'

" Mouse
set mouse=nicr
set mouse=a

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

" Gvim
set guifont=Fira\ Code
let g:neovide_transparency=0.8
" Neovide
" Smooth Scrolling
" let g:NEOVIDE_MULTIGRID=true
" Transparency
let g:neovide_transparency=0.8
let g:neovide_refresh_rate=144
