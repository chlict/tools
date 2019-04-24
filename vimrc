" shut down tips sound.
set vb t_tb=
set nocompatible
set nu
set history=1000
set showmatch
set guioptions-=T
set ruler
set nohls
set cindent
set incsearch
" don't wrap a long line.
" set nowrap

" replace tab with space.
set expandtab
" four space.
set tabstop=4
" width of autoindent.
set shiftwidth=4

syntax on
" filetype indent on
set autoindent

" highlight the text to search.
set hls
set ignorecase

" control the cursor with mouse.
" set mouse=a

set nobackup

map  <F2> :e%<Enter>
imap <F2> <Esc>:e%<Enter>

" quick save file.
map  <F3> :w<Enter>
imap <F3> <Esc>:w<Enter>

" window manager.
" let g:winManagerWindowLayout='FileExplorer|TagList'
" map  <F4> :WMToggle<cr>
" imap <f4> <Esc>:WMToggle<cr>

" run script.
map  <F5> <F3>:!./%<Enter>
imap <F5> <F3>:!./%<Enter>

" create ctags file.
map  <F6> <F3>:!ctags -R<Enter>
imap <F6> <F3>:!ctags -R<Enter>

map  <F7> <F3>:!make<Enter>
imap <F7> <F3>:!make<Enter>

" intelligent tab.
inoremap <F8> <C-x><C-o>

" comment python
map cp ^i#<Esc>

" uncomment python
map ucp ^i<Del><Esc>

" pydiction
" autocmd FileType python set complete+=k~/.vim/tools/pydiction

" comment c++
map cc ^i//<Esc>

" uncomment c++
map ucc ^i<Del><Del><Esc>

map \inc i#include<Esc>

map \class iclass<Enter>{<Enter>public:<Home><Del><Del><Del><Del><End><Enter>};<Esc>

map \if iif ()<Enter>{<Enter>}<Esc>
map \ife iif ()<Enter>{<Enter>}<Enter>else<Enter>{<Enter>}<Esc>

map \while iwhile ()<Enter>{<Enter>}<Esc>

map \for ifor ()<Enter>{<Enter>}<Esc>

map \enum ienum<Enter>{<Enter>}<Esc>

map \comment i/**<Enter>@brief<Enter>@param[in]<Enter>@param[out]<Enter>@return<Enter>*/<Esc>

