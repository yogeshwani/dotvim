" To disable a plugin, add it's bundle name to the following list
let g:pathogen_disabled = []

"if v:version < '702'
    "call add(g:pathogen_disabled, 'fuzzyfinder')
    "call add(g:pathogen_disabled, 'l9')
    call add(g:pathogen_disabled, 'clang-complete')
"endif
call pathogen#infect()
syntax on
set nocompatible
set nobackup
set nowritebackup
"set noswapfile
set directory^=$HOME/tmp

set title
set nowrap
set tabstop=4
set expandtab
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
nmap <F8> :TagbarToggle<CR>
nmap <F9> :FufFile<CR>

set laststatus=2

if &diff
    colorscheme greens
endif

nnoremap z/ :if AutoHighlightToggle()<Bar>set hls<Bar>endif<CR>
function! AutoHighlightToggle()
  let @/ = ''
  if exists('#auto_highlight')
    au! auto_highlight
    augroup! auto_highlight
    setl updatetime=4000
    echo 'Highlight current word: off'
    return 0
  else
    augroup auto_highlight
      au!
      au CursorHold * let @/ = '\V\<'.escape(expand('<cword>'), '\').'\>'
    augroup end
    setl updatetime=500
    echo 'Highlight current word: ON'
    return 1
  endif
endfunction

"folding settings
set foldmethod=indent   "fold based on indent
set foldnestmax=10      "deepest fold is 10 levels
set nofoldenable        "dont fold by default
set foldlevel=1         "this is just what i use
