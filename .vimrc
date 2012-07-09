set nocompatible
set nobackup
set nowritebackup
set noswapfile

set nowrap
set tabstop=4
set shiftwidth=4
set shiftround
set autoindent
set copyindent

set hlsearch
set incsearch
set ignorecase
set smartcase

set hidden
set showcmd

set list
set listchars=tab:>.,trail:.,extends:#,nbsp:.

filetype plugin indent on

call pathogen#helptags()
call pathogen#runtime_append_all_bundles()

"let Tlist_Show_Menu=1

"let Tlist_Ctags_Cmd = "/usr/bin/ctags"
"let Tlist_WinWidth = 50
"map <F4> :TlistToggle<cr>
"map <F8> :!/usr/bin/ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>

"let loaded_minibufexplorer = 1

"let g:miniBufExplMapWindowNavVim = 1 
"let g:miniBufExplMapWindowNavArrows = 1 
"let g:miniBufExplMapCTabSwitchBufs = 1 
"let g:miniBufExplModSelTarget = 1 

