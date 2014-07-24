" Vundle configuration
" -----------------------------------------------------------------------------

set nocompatible
filetype off

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Vundle management (for updates)
Plugin 'gmarik/vundle'

" List of bundles (plugins)
" -----------------------------------------------------------------------------

filetype plugin indent on

" YouCompleteMe setup
" Plugin 'Valloric/YouCompleteMe'
" let g:ycm_semantic_triggers = {'haskell' : ['.']}
" let g:ycm_global_ycm_extra_conf = '~/.vim/bundle/YouCompleteMe/third_party/ycmd/cpp/ycm/.ycm_extra_conf.py'

Plugin 'tpope/vim-fugitive'

Plugin 'tpope/vim-surround'

Plugin 'tpope/vim-markdown'

Plugin 'godlygeek/tabular'

Plugin 'scrooloose/nerdcommenter'

" NERDTree setup
Plugin 'scrooloose/nerdtree'
map <F2> <Esc>:NERDTreeToggle<CR>

" Python-mode setup
Plugin 'klen/python-mode'
" let g:pymode_rope_complete_on_dot = 0
let g:pymod_run=1
let g:pymode_folding=1
let g:pymode_options=1
let g:pymode_syntax=1
let g:pymode_syntax_all=1
let g:pymode_syntax_slow_sync=1
let g:pymode_trim_whitespaces=0
let g:pymode_lint=0
let g:pymode_doc=0
let g:pymode_rope=0
let g:pymode_rope_lookup_project=0

Plugin 'vim-scripts/Align'

" Airline setup
Plugin 'bling/vim-airline'
set laststatus=2
let g:airline_powerline_fonst=1
let g:airline#extensions#tabline#enabled=1
if has("gui_running")
    let g:airline_theme='solarized'
    " powerline symbols
    let g:airline_left_sep = ''
    "let g:airline_left_alt_sep = ''
    let g:airline_right_sep = ''
    "let g:airline_right_alt_sep = ''
    let g:airline_symbols.branch = ''
    let g:airline_symbols.readonly = ''
    let g:airline_symbols.linenr = ''
else
    let g:airline_theme='solarized'
    let g:airline_left_sep = ' '
    let g:airline_right_sep = ' '
    let g:airline_left_alt_sep = ' '
    let g:airline_right_alt_sep = ' '
    "let g:airline_symbols.branch = ' '
    "let g:airline_symbols.readonly = ' '
    "let g:airline_symbols.linenr = ' '
endif

Plugin 'altercation/vim-colors-solarized'

Plugin 'jnurmine/Zenburn'

Plugin 'sickill/vim-monokai'

" Jellybeans setup
Plugin 'nanotech/jellybeans.vim'
let g:jellybeans_use_lowcolor_black = 0

" Molokai theme setup
Plugin 'tomasr/molokai'
let g:molokai_original=1
let g:rehash256=1

Plugin 'eagletmt/ghcmod-vim'

Plugin 'eagletmt/neco-ghc'

" Vim2hs setup
" Plugin 'dag/vim2hs'
" if has("gui_running")
"    let g:haskell_conceal_wide = 1
" endif

Plugin 'Shougo/vimproc.vim'

Plugin 'Shougo/vimshell.vim'

Plugin 'Shougo/unite.vim'

" Syntastic setup
Plugin 'scrooloose/syntastic'
let g:syntastic_c_check_header=1
let g:syntastic_c_no_include_search=1
let g:syntastic_c_no_default_include_dirs=1
let g:syntastic_c_auto_refresh_includes=1
let g:syntastic_c_compiler_options='-Wall'
let g:syntastic_cpp_check_header = 1
let g:syntastic_cpp_no_include_search = 1
let g:syntastic_cpp_no_default_include_dirs = 1
let g:syntastic_cpp_auto_refresh_includes = 1
let g:syntastic_cpp_compiler_options = '-Wall -std=c++0x'

" Neocomplete setup
Plugin 'Shougo/neocomplete.vim'
let g:neocomplete#enable_at_startup=1
let g:neocomplete#enable_smart_case=1
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
    return neocomplete#close_popup() . "\<CR>"
endfunction

" CtrlP setup
Plugin 'kien/ctrlp.vim'
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

Plugin 'jimenezrick/vimerl'

" Tagbar setup
Plugin 'majutsushi/tagbar'
nmap <F8> :TagbarToggle<CR>

" General
" -----------------------------------------------------------------------------

autocmd BufNewFile,BufReadPost *.md set filetype=markdown

inoremap kj <ESC>

nmap <PageUp> :bn<CR>
nmap <PageDown> :bp<CR>
nmap <Del> :bd<CR>

noremap <Up> <C-W>k
noremap <Down> <C-W>j
noremap <Left> <C-W>h
noremap <Right> <C-W>l

colorscheme solarized

let mapleader = ','

set modelines=0
set background=dark
set t_Co=256

set wildmenu
set wildignorecase

syntax on

" affichage des numeros de lignes
set number

" longueur maximale des lignes
set wrap
set textwidth=80
set cc=+1

set guifont=Droid\ Sans\ Mono

set showcmd    " Show (partial) command in status line.
set showmatch  " Show matching brackets.
set ignorecase " Do case insensitive matching
set smartcase  " Do smart case matching
set incsearch  " Incremental search
set autowrite  " Automatically save before commands like :next and :make
set hidden     " Hide buffers when they are abandoned
set history=50 " Keep 50 lines of history
set ruler      " Show the cursor position all the time
set hlsearch   " Highlight search results

set encoding=utf-8
set scrolloff=3
set autoindent
set showmode

set ttyfast
set ruler
set laststatus=2

set norelativenumber

set shell=/bin/bash

" Enable mouse usage (all modes)
if has('mouse')
        set mouse=a
endif

" TAB settings
set tabstop=4                   "A tab is 4 spaces
set expandtab                   "Always uses spaces instead of tabs
set softtabstop=4               "Insert 4 spaces when tab is pressed
set shiftwidth=4                "An indent is 4 spaces
set smarttab                    "Indent instead of tab at start of line
set shiftround                  "Round spaces to nearest shiftwidth multiple
set nojoinspaces                "Don't convert spaces to tabs

" Dictionary path, from which the words are being looked up
set dictionary=/usr/share/dict/words

set listchars=tab:▸\ ,eol:¬

