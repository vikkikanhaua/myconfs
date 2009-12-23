"
" ~/.vimrc
"
""""""""""""

colorscheme slate

" Main options {{{ 
set t_Co=256
set nocompatible
set autoindent
set backspace=indent,eol,start
set expandtab
set foldmethod=syntax
set history=50
set incsearch
set ignorecase
set smartcase
set mouse=v
set nobackup
set hlsearch
set nomousehide
set wrap
set number
set ruler
set shiftwidth=2
set shortmess+=r
set showmode
set showcmd
set showtabline=1
set smartindent
set smarttab
set title
set vb t_vb=
set wildmode=list:longest,full

syntax on
filetype plugin indent on

" }}}

" Folding stuffs {{{
if has ('folding')
  set foldenable
  set foldmethod=marker
  set foldmarker={{{,}}}
  set foldcolumn=0
endif

" }}}

" File type specific options {{{
augroup FILES
  au FileType c      set formatoptions+=ro
  au FileType make   set noexpandtab shiftwidth=8
  au FileType python set expandtab shiftwidth=2 tabstop=2
  au FileType c      syn match matchName /\(#define\)\@<= .*/
  au FileType cpp    syn match matchName /\(#define\)\@<= .*/
  au FileType text   setlocal textwidth=76
augroup END

let python_highlight_all = 1
let python_highlight_space_errors = 1
let python_fold=1
let lua_fold=1
let lua_version = 5
let lua_subversion = 1

" haskell stuff:
" au Bufenter *.hs compiler ghc

let g:haddock_browser = "/usr/bin/firefox"

" }}}

" Keymaps {{{
" unmap annoying keys
nnoremap q: <Nop>
nnoremap q/ <Nop>
nnoremap q? <Nop>

" leader key
:let mapleader = ","
nnoremap <Leader>u :diffupdate<cr>
nnoremap <Leader>g :diffget<cr>
nnoremap <Leader>p :diffput<cr> 

" 'transpose' key
nmap t xp

" Custom keys
map ; :
nnoremap <space> za
noremap <f1> :bnext<CR>

" }}}

" Option toggle function {{{
function MapToggle(key, opt)
  let cmd = ':set '.a:opt.'! \| set '.a:opt."?\<CR>"
  exec 'nnoremap '.a:key.' '.cmd
  exec 'inoremap '.a:key." \<C-O>".cmd
endfunction
command -nargs=+ MapToggle call MapToggle(<f-args>)

" And the commands to toggle
MapToggle <F5> number
MapToggle <F6> spell
MapToggle <F7> paste
MapToggle <F8> hlsearch
MapToggle <F9> wrap

" }}}

" Comment function {{{
" Comment a visual block
function CommentLines()
    execute ":s@^@".g:StartComment." @g"
    "execute ":s@^\\(\\s*\\)@\\1".g:StartComment." @g"

    execute ":s@$@ ".g:EndComment."@g"
endfunction

" Uncomment a visual block
function UncommentLines()
    execute ":s@^".g:StartComment." @\@g"
    "execute ":s@^\\(\\s*\\)".g:StartComment." @\\1@g"

    execute ":s@ ".g:EndComment."$@@g"
endfunction

" Set comment characters for common languages
autocmd FileType python,sh,bash,zsh,ruby,perl let StartComment="#"  | let EndComment=""
autocmd FileType php,c,javascript             let StartComment="//" | let EndComment=""

autocmd FileType html    let StartComment="<!--" | let EndComment="-->"
autocmd FileType cpp     let StartComment="/*"   | let EndComment="*/"
autocmd FileType haskell let StartComment="--"   | let EndComment=""
autocmd FileType vim     let StartComment="\""   | let EndComment=""

vmap ,c :call CommentLines()<cr>
vmap ,u :call UncommentLines()<cr>

" }}}

" Retain last known cursor position {{{
augroup LAST
  au BufReadPost *
    \ if line("'\"") > 1 && line("'\"") <= line("$") |
    \   exe "normal! g`\"" |
    \ endif
augroup END

" }}}

" File types for nonstandard/additional config files {{{
augroup SYNTAX
  au BufNewFile,BufRead *conkyrc*    set ft=conkyrc
  au BufNewFile,BufRead *muttrc*     set ft=muttrc
  au BufNewFile,BufRead *.rem        set ft=remind
  au BufNewFile,BufRead *screenrc*   set ft=screen
augroup END

" }}}

" Some tricks to format paragraphs in mutt {{{
augroup MUTT
  au BufRead ~/.mutt/temp/mutt* nmap  <F1>  gqap
  au BufRead ~/.mutt/temp/mutt* nmap  <F2>  gqqj
  au BufRead ~/.mutt/temp/mutt* nmap  <F3>  kgqj
  au BufRead ~/.mutt/temp/mutt* map!  <F1>  <ESC>gqapi
  au BufRead ~/.mutt/temp/mutt* map!  <F2>  <ESC>gqqji
  au BufRead ~/.mutt/temp/mutt* map!  <F3>  <ESC>kgqji
augroup END

" }}}
