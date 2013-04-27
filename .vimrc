"
" ~/.vimrc
"
""""""""""""

" Main options {{{
if $TERM =~ "256"
  set t_Co=256
  colorscheme zencustom
else
  set t_Co=16
  colorscheme delek
endif

set nocompatible
set autoindent
set expandtab
set incsearch
set ignorecase
set smartcase
set nobackup
set hlsearch
set nowrap
set number
set confirm
set ruler
set showmode
set showcmd
set smartindent
set smarttab
set linebreak
set title
set vb t_vb=
set shiftwidth=2
set showtabline=1
set laststatus=2
set shortmess=a
set history=50
set updatetime=500
set mouse=nvcr
set foldmethod=syntax
set formatoptions=tcroqn1
set backspace=indent,eol,start
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
" Keymaps {{{
" unmap annoying keys
nnoremap q: <Nop>
nnoremap q/ <Nop>
nnoremap q? <Nop>

" leader key
:let mapleader = ","

" diff
nnoremap <Leader>u :diffupdate<cr>
nnoremap <Leader>g :diffget<cr>
nnoremap <Leader>p :diffput<cr>

" comment/uncomment a visual block
vmap <Leader>c :call CommentLines()<CR>

" macro key
:nnoremap <F2> @q

map ; :
map <space> za

" Tab indents code in visual mode
vmap <Tab> >gv
vmap <S-Tab> <gv

cmap w!! %!sudo tee > /dev/null %

" Tab controls
map <Leader><Tab> :tabnew
map <Leader>o     :tabnext<CR>
map <Leader>a     :tabprevious<CR>

" }}}
" Autocommands {{{
if has('autocmd')
  let python_highlight_all = 1
  let python_highlight_space_errors = 1
  let python_fold = 1
  let lua_fold = 1
  let lua_version = 5
  let lua_subversion = 1

  " html
  au Filetype html,xml,xsl set spell

  " Remove trailing whitespace
  " au BufWritePre * :call setline(1, map(getline(1, "$"), 'substitute(v:val, "\\s\\+$", "","")'))

  " set the title string
  au BufEnter * let &titlestring = "vim: " . substitute(expand("%:p"), $HOME, "~", '')
  au BufEnter * let &titleold    = substitute(getcwd(), $HOME, "~", '')

  " set a better status line
  au BufRead * call SetStatusLine()

  " restore cursor position
  au BufReadPost * call RestoreCursorPos()
  "au BufWinEnter * call OpenFoldOnRestore()

  " file types for nonstandard/additional config files
  au BufNewFile,BufRead *conkyrc*          set ft=conkyrc
  au BufNewFile,BufRead *muttrc*           set ft=muttrc
  au BufNewFile,BufRead *.rem              set ft=remind
  au BufNewFile,BufRead $SCREEN_CONF_DIR/* set ft=screen
  au BufNewFile,BufRead *.xcolors          set ft=xdefaults
  au BufNewFile,BufRead *.rss              set ft=xml

  " change how vim behaves when composing emails
  au BufNewFile,BufRead ~/.mutt/temp/mutt* set ft=mail | set textwidth=72 | set spell | set nohls

  au BufNewFile,BufRead ~/.mutt/temp/mutt* nmap  <F1>  gqap
  au BufNewFile,BufRead ~/.mutt/temp/mutt* nmap  <F2>  gqqj
  au BufNewFile,BufRead ~/.mutt/temp/mutt* nmap  <F3>  kgqj
  au BufNewFile,BufRead ~/.mutt/temp/mutt* map!  <F1>  <ESC>gqapi
  au BufNewFile,BufRead ~/.mutt/temp/mutt* map!  <F2>  <ESC>gqqji
  au BufNewFile,BufRead ~/.mutt/temp/mutt* map!  <F3>  <ESC>kgqji

  " set comment characters for common languages
  au FileType python,sh,bash,zsh,ruby,perl     let StartComment="#"  | let EndComment=""
  au FileType cpp,php,c,javascript             let StartComment="//" | let EndComment=""

  au FileType html    let StartComment="<!--" | let EndComment="-->"
  au FileType haskell let StartComment="--"   | let EndComment=""
  au FileType lua     let StartComment="--"   | let EndComment=""
  au FileType vim     let StartComment="\""   | let EndComment=""

  " file type specific commands
  au FileType c      set formatoptions+=ro
  au FileType make   set noexpandtab shiftwidth=8
  au FileType python set expandtab shiftwidth=2 tabstop=2
  au FileType c      syn match matchName /\(#define\)\@<= .*/
  au FileType cpp    syn match matchName /\(#define\)\@<= .*/
  au FileType text   setlocal textwidth=72
endif
" }}}
" Functions {{{
function! SetStatusLine()
    let l:s1="%-3.3n\\ %f\\ %h%m%r%w"
    let l:s2="[%{strlen(&filetype)?&filetype:'?'},\\ %{&encoding},\\ %{&fileformat}]"
    let l:s3="%=\\ 0x%-8B\\ \\ %-14.(%l,%c%V%)\\ %<%P"
    execute "set statusline=" . l:s1 . l:s2 . l:s3
endfunction

function! RestoreCursorPos()
  if expand("<afile>:p:h") !=? $TEMP
    if line("'\"") > 1 && line("'\"") <= line("$")
      let line_num = line("'\"")
      let b:doopenfold = 1
      if (foldlevel(line_num) > foldlevel(line_num - 1))
        let line_num = line_num - 1
        let b:doopenfold = 2
      endif
      execute line_num
    endif
  endif
endfunction

function! OpenFoldOnRestore()
  if exists("b:doopenfold")
    execute "normal zv"
    if(b:doopenfold > 1)
      execute "+".1
    endif
    unlet b:doopenfold
  endif
endfunction

function CommentLines()
  try
    execute ":s@^".g:StartComment." @\@g"
    execute ":s@ ".g:EndComment."$@@g"
  catch
    execute ":s@^@".g:StartComment." @g"
    execute ":s@$@ ".g:EndComment."@g"
  endtry
endfunction

function! s:DiffWithSaved()
  let filetype=&ft
  diffthis
  vnew | r # | normal! 1Gdd
  diffthis
  exe "setlocal bt=nofile bh=wipe nobl noswf ro ft=" . filetype
endfunction
com! DiffSaved call s:DiffWithSaved()

function MapToggle(key, opt)
  let cmd = ':set '.a:opt.'! \| set '.a:opt."?\<CR>"
  exec 'nnoremap '.a:key.' '.cmd
  exec 'inoremap '.a:key." \<C-O>".cmd
endfunction

command -nargs=+ MapToggle call MapToggle(<f-args>)

MapToggle <F4> foldenable
MapToggle <F5> number
MapToggle <F6> spell
MapToggle <F7> paste
MapToggle <F8> hlsearch
MapToggle <F9> wrap

" }}}
