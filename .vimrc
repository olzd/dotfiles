" All system-wide defaults are set in $VIMRUNTIME/debian.vim (usually just
" /usr/share/vim/vimcurrent/debian.vim) and sourced by the call to :runtime
" you can find below.  If you wish to change any of those settings, you should
" do it in this file (/etc/vim/vimrc), since debian.vim will be overwritten
" everytime an upgrade of the vim packages is performed.  It is recommended to
" make changes after sourcing debian.vim since it alters the value of the
" 'compatible' option.

" This line should not be removed as it ensures that various options are
" properly set to work with the Vim-related packages available in Debian.
runtime! debian.vim

" Uncomment the next line to make Vim more Vi-compatible
" NOTE: debian.vim sets 'nocompatible'.  Setting 'compatible' changes numerous
" options, so any other options should be set AFTER setting 'compatible'.
"set compatible
set nocompatible
set modelines=0

" Vim5 and later versions support syntax highlighting. Uncommenting the next
" line enables syntax highlighting by default.
syntax on

" If using a dark background within the editing area and syntax highlighting
" turn on this option as well
set background=dark

" Set GUI font to Droid Sans Mono
set guifont=Droid\ Sans\ Mono

" Uncomment the following to have Vim jump to the last position when
" reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g'\"" | endif
endif

nnoremap g; g;zz

inoremap jk <ESC>

" Uncomment the following to have Vim load indentation rules and plugins
" according to the detected filetype.
if has("autocmd")
  filetype plugin indent on
endif

" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
set number		" Show line number
set showcmd		" Show (partial) command in status line.
set showmatch		" Show matching brackets.
set ignorecase		" Do case insensitive matching
set smartcase		" Do smart case matching
set incsearch		" Incremental search
set autowrite		" Automatically save before commands like :next and :make
set hidden             " Hide buffers when they are abandoned
set history=50		" Keep 50 lines of history
set ruler		" Show the cursor position all the time
set hlsearch		" Highlight search results
"
set encoding=utf-8
set scrolloff=3
set autoindent
set showmode
"
set ttyfast
set ruler
set laststatus=2
"
set norelativenumber
"
set shell=/bin/bash

" Enable mouse usage (all modes)
if has('mouse')
	set mouse=a
endif

" Source a global configuration file if available
if filereadable("/etc/vim/vimrc.local")
  source /etc/vim/vimrc.local
endif

" let xterm16_termtype = 'rxvt'

au VimEnter *
	\ if &term == 'xterm' || &term == 'rxvt'	|
	\ 	set t_Co=256				|
	\ endif

" Select colormap : 'soft', 'soflight', 'standard' or 'allblue'
let xterm16_colormap = 'allblue'

" Select brightness : 'low', 'med', 'high' or 'default'
let xterm16_brightness = 'high'

colo xterm16
" colo jellybeans
" colo lettuce

" TAB settings
" set tabstop=4
set shiftwidth=4
set softtabstop=4
set expandtab
" set smarttab
set shiftround
set nojoinspaces

" Changing Leader Key
let mapleader = ","

" Set title to window
set title

" Dictionary path, from which the words are being looked up
set dictionary=/usr/share/dict/words

" Make pasting done without any indentation break
set pastetoggle=<F3>

" Make Vim able to edit crontab files again
"set backupskip=/tmp/*,/private/tmp/*

" Make Vim to handle long lines nicely
set wrap
set textwidth=79
set formatoptions-=t
set formatoptions-=c
set formatoptions-=o
set formatoptions-=l
set formatoptions=qrn1
set colorcolumn=0
" To show special characters in Vim
"set list
set listchars=tab:▸\ ,eol:¬

" Navigation using keys up/down/left/right
" Disabling default keys to learn the hjkl thing
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
nnoremap j gj
nnoremap k gk

" Get rid of those help keys
inoremap <F1> <ESC>
nnoremap <F1> <ESC>
vnoremap <F1> <ESC>

" Working with split screens nicely
" Resize Split when the window is resized
au VimResized * :wincmd =

" Wildmenu completion
set wildmenu
set wildmode=list:longest
set wildignore+=.hg,.svn "Version Controls"
set wildignore+=*.aux,*.out,*.toc "Latex Intermediate files"
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg "Binary Imgs"
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest "Compiled Object files"
set wildignore+=*.spl "Compiled speolling world list"
set wildignore+=*.sw? "Vim swap files"
set wildignore+=*.DS_Store "OSX SHIT"
set wildignore+=*.luac "Lua byte code"
set wildignore+=migrations "Django migrations"
set wildignore+=*.pyc "Python Object codes"
set wildignore+=*.orig "Merge resolution files"

" Set omnicompletion
set ofu=syntaxcomplete#Complete

" ========== Plugins Settings ==========

" Mapping to NERDTree
let NERDChristmasTree=1 " Set to 1 for fancy colors
let NERDTreeMinimalUI=0 " Disable the 'Press ? for help' text

nnoremap <C-n> :NERDTreeToggle<cr>

" MiniBuffer settings
let g:miniBufExplMapWindowNavVim = 1
let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplModelTarget = 1

" Tagbar key bindings
nmap <leader>l <ESC>:TagbarToggle<cr>
imap <leader>l <ESC>:TagbarToggle<cr>i

" LaTeX plugin
set grepprg=grep\ -nH\ $* " Set grep program to always generate a file-name
let g:tex_flavor='latex'

" ========== END Plugins Settings ==========
