" Vim Configuration

" TODO in main config, include prettier/eslint etc for JS/TS a base install
" TODO same for clj-kondo et al.
" TODO word wrap at like.. 100/120 width instead of 80

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins (and remove all existing autocmds)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
autocmd!

func! EnsureManager()
  let plugPath = expand("~/.vim/autoload/plug.vim")
  if !filereadable(plugPath)
    echo 'downloading plugin manager'
    echo system("curl -fLo " . plugPath . " --create-dirs "
          \. "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim")
  endif
endfunc
call EnsureManager()
call plug#begin('~/.vim/plugged')

Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-fireplace', { 'for': ['clojure'] }
Plug 'MaxMEllon/vim-jsx-pretty'
Plug 'leafgarland/typescript-vim'
Plug 'Quramy/tsuquyomi'
Plug 'dense-analysis/ale' " lsp + linting
" TODO include colorscheme in it's own git project to import here.

call plug#end()

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" General Editor Configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
syn on
filetype plugin indent on
set nocompatible nu autowrite hidden shiftwidth=2 tabstop=2 gdefault mouse=a
set clipboard=unnamed,unnamedplus expandtab smarttab ignorecase smartcase
set iskeyword+=- path+=** wildmode=longest,list wildmenu noswapfile textwidth=99
set stl=--\ %1*%F%m%r%h%w%*\ %=\ %y\ -\ [%l,%c]\ [%L,%p%%] showtabline=1
if v:version >= 800 | set shortmess+=c | endif
set hlsearch cot+=preview scrolloff=3 nobackup nowritebackup
set backspace=indent,eol,start showcmd
set fillchars=stlnc:\-,stl:\-,vert:\|
set modeline modelines=3 foldmethod=manual nofoldenable nojoinspaces autoread
set timeout timeoutlen=1000 ttimeoutlen=100
set diffopt=vertical signcolumn=no
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set completeopt=menu,preview

" Use persistent undo history.
if !isdirectory("/tmp/.vim-undo-dir")
    call mkdir("/tmp/.vim-undo-dir", "", 0700)
endif
set undodir=/tmp/.vim-undo-dir
set undofile

let g:sh_noisk=1 "stop vim messing with iskeyword when opening a shell file

syntax on
filetype plugin indent on

setglobal grepformat=%f:%l:%c:%m,%f:%l:%m,%f:%l%m,%f\ \ %l%m
if executable('rg')
  setglobal grepprg=rg\ -s\ --vimgrep
elseif has('unix')
  " . will search for everything, remove if you want .clj etc
  setglobal grepprg=grep\ -rn\ $*\ .\ /dev/null
endif
setglobal tags=./tags;
runtime macros/matchit.vim
let g:ftplugin_sql_omni_key = '<Nop>' " ctrl+c is for escape, not completion.

colorscheme tailstone

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Language Specific Configuration
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

augroup language
  autocmd!

  au Syntax clojure nmap <buffer> gd <Plug>FireplaceDjump

  " Leave the return key alone when in command line windows, since it's used
  " to run commands there.
  autocmd! CmdwinEnter * :unmap <cr>
  autocmd! CmdwinLeave * :call MapCR()

  " javascript
  autocmd! FileType javascript set sw=2 sts=2 expandtab
  " Two-space indents in json
  autocmd! FileType json set sw=2 sts=2 expandtab

  " Compute syntax highlighting from beginning of file. (By default, vim only
  " looks 200 lines back, which can make it highlight code incorrectly in some
  " long files.)
  autocmd BufEnter * :syntax sync fromstart

  au Syntax typescript nmap <buffer> gd :TsuDefinition<cr>
  au Syntax typescript.tsx nmap <buffer> gd :TsuDefinition<cr>
  au Syntax typescript nmap <buffer> gD :TsuTypeDefinition<cr>
  au Syntax typescript.tsx nmap <buffer> gD :TsuTypeDefinition<cr>

  " Vim 8.2 adds built-in JSX support which seems broken. Setting these
  " filetypes lets the installed plugins deal with JSX/TSX instead.
  autocmd bufnewfile,bufread *.tsx set filetype=typescript.tsx
  autocmd bufnewfile,bufread *.jsx set filetype=javascript.jsx
augroup END

let g:ale_lint_on_text_changed = 'normal'
let g:ale_lint_on_insert_leave = 1
let g:ale_lint_delay = 0
let g:ale_set_quickfix = 0
let g:ale_set_loclist = 0
let g:ale_javascript_eslint_executable = 'eslint --cache'
let g:ale_linters = {
      \'clojure': ['clj-kondo'],
      \'javascript': ['eslint'],
      \'typescript': ['tsserver', 'eslint'], 
      \'typescript.tsx': ['tsserver', 'eslint',]
      \}
let g:ale_fixers = {
      \'typescript': ['prettier'],
      \'typescript.tsx': ['prettier']
      \}
" Vim-ale handles TypeScript quickfix, so tell Tsuquyomi not to do it.
let g:tsuquyomi_disable_quickfix = 1

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Simple Keybinds
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader=' '
nnoremap Y y$
nnoremap Q @q
nnoremap gb :Git blame<cr>
nnoremap <C-q> :quit<cr>
nnoremap <leader>h :LSClientShowHover<CR>
if executable('selecta')
  " nnoremap <leader>f :call SelectaCommand("find * -type f", "", ":e")<cr>
  nnoremap <leader>f :call SelectaFile("", ":e")<cr>
else
  nnoremap <leader>f :find<space>
endif
nnoremap <Leader>e :e <C-R>=expand("%:p:h") . '/'<CR>
nnoremap <leader>g :grep<space>
nnoremap <leader>l :e ~/.log.md<cr>
nnoremap <leader>n :e ~/.notes.md<cr>
nnoremap <leader>b :b<space>

" nnoremap <Tab> <C-^>

imap <C-c> <esc>
map <C-h> <C-w><C-h>
map <C-j> <C-w><C-j>
map <C-k> <C-w><C-k>
map <C-l> <C-w><C-l>
cnoremap <C-f> <Right>
cnoremap <C-b> <Left>
cnoremap <C-a> <Home>
cnoremap <C-e> <End>
cnoremap <C-d> <Delete>
cnoremap <expr> %% expand('%:h').'/'
inoremap <C-f> <Right>
inoremap <C-b> <Left>
inoremap <C-d> <Delete>
inoremap <C-l> <space>=><space>
nnoremap <leader><leader> <c-^>
nnoremap gF :ALEFix<cr>
nnoremap gj :ALENextWrap<cr>
nnoremap gk :ALEPreviousWrap<cr>
nnoremap g1 :ALEFirst<cr>
nnoremap g0 :ALEStopAllLSPs<cr>
nnoremap gq :copen<cr>


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Custom functions & their associated keybinds
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Multi purpose Tab key
function! InsertTabWrapper()
    let col = col('.') - 1
    if !col
        return "\<tab>"
    endif

    let char = getline('.')[col - 1]
    if char =~ '\k'
        " There's an identifier before the cursor, so complete the identifier.
        return "\<c-p>"
    else
        return "\<tab>"
    endif
endfunction
inoremap <expr> <tab> InsertTabWrapper()
inoremap <s-tab> <c-n>

function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction
map <leader>r :call RenameFile()<cr>

function! OpenTestAlternate()
endfunction
nnoremap <leader>. :call OpenTestAlternate()<cr>

function! MapCR()
  nnoremap <cr> :call RunFile()<cr>
endfunction
call MapCR()

" CR for general run this
" Shift + CR for run the last test file that was run

" If we've previously run a test in our vim instance, we'll prefer running the last test file
let g:test_mode = 0
let g:last_clj_test_ns = ''
let g:last_test_file = ''

function! RunFile(...)
  if a:0
    let command_suffix = a:1
  else
    let command_suffix = ""
  endif

  " Are we in a test file?
  let in_test_file = match(expand("%"), '\(_test.clj\|_test.cljc\|_test.cljs\|test_.*\.py\|_test.py\|.test.ts\|.test.ts\)$') != -1

  if expand("%") != ""
    " avoid printing 2 lines and requiring the user to hit CR after each run.
    silent :w

    if &filetype == 'clojure'
      silent :Require
    endif
  endif

  if in_test_file || g:test_mode == 1
    let g:test_mode = 1

    if in_test_file
      let g:last_test_file = expand("%")
      if &filetype == 'clojure'
        let g:last_clj_test_ns = g:fireplace#ns()
      endif
    endif

    if &filetype == 'clojure'
      " will fail if there is no live REPL via fireplace
      if expand('%:e') == 'cljs'
        echo fireplace#cljs().Query("(with-out-str (cljs.test/run-tests '" . g:last_clj_test_ns . '))')
      else
        execute('RunTests ' . g:last_clj_test_ns)
      endif
    else
      echo "No test run configured for filetype:" &filetype
    endif
  else
    if &filetype == 'clojure'
      echom "I would send the file to it's interpreter"
    elseif &filetype == 'python'
      :!python -i %
    elseif &filetype == 'sh'
      :!./%
    elseif &filetype == 'javascript'
      :!node -i -e "$(< %)"
    else
      echom "No run configured for filetype:" &filetype
    endif
  endif
endfunction

" Like Emacs, clear certain states (e.g hlsearch) and pass through C-g behaviour.
nnoremap <silent><expr> <C-g> (v:count ? ':<C-u>:call <SID>save_change_marks()\|edit\|call <SID>restore_change_marks()<CR>' : '')
      \ . ':nohlsearch'.(has('diff')?'\|diffupdate':'')
      \ . '<cr><C-l><C-g>'

command! TrimWhitespace :%s/\s\+$//e
command! ClearPrefixWhitespace :%s/^\s\+//g
command! PrettifyJSON :%!python -m json.tool
command! PrettifyXML  :%!xmllint --format -
" TODO how else would we eval and find the values of vars? TDD in frontend?
command! JackInCljs :CljEval (figwheel.main.api/cljs-repl "dev")<cr>
command! DR :CljEval (dev/reset)<cr>
nnoremap zS :echo join(reverse(map(synstack(line('.'), col('.')), 'synIDattr(v:val,"name")')),' ')<cr>

" Run a given vim command on the results of fuzzy selecting from a given shell
" command. See usage below.
function! SelectaCommand(choice_command, selecta_args, vim_command)
  try
    let selection = system(a:choice_command . " | selecta " . a:selecta_args)
  catch /Vim:Interrupt/
    " Swallow the ^C so that the redraw below happens; otherwise there will be
    " leftovers from selecta on the screen
    redraw!
    return
  endtry
  redraw!
  exec a:vim_command . " " . selection
endfunction

function! SelectaFile(path, command)
  if executable('fd')
    call SelectaCommand("fd -t f . " . a:path, "", a:command)
  else 
    if getcwd() == expand('~')
      " Allow us to search our config stored as a bare repo in the home directory.
      call SelectaCommand("git --git-dir=" . expand('~') . "/.cfg ls-files", "", ":e")
    else
      call SelectaCommand("find * -type f", "", ":e")
    endif
  endif
  " if executable('fd')
  "   nnoremap <leader>f :call SelectaFile(".", "*", ":edit")<cr>
  " else
  "   nnoremap <leader>f :call SelectaCommand("find * -type f", "", ":e")<cr>
  " endif
  " call SelectaCommand("fd -t f . " . a:path, "", a:command)
endfunction
