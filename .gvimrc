" ========== Gvim Settings ==========

" Removing scrollbars
if has("gui_running")
	set guitablabel=%-0.12t%M
	set guioptions-=T
	set guioptions-=r
	set guioptions-=L
	set guioptions +=a
	set guioptions-=m
    set guioptions-=b
    set guiheadroom=0
	set listchars=tab:▸\ ,eol:¬  " Hum..
endif

"set ghr=0
"set go -=T
"set go -=r
"set tb=

" Source the .vimrc file after saving it
" autocmd bufwritepost .vimrc source ~/.vimrc
