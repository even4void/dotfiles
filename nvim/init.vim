" Basic setup
set encoding=utf-8                     " encoding
set fileencoding=utf-8
set fileformats=unix,dos,mac
set backspace=indent,eol,start         " fix backspace indent
set tabstop=4                          " indentation rules
set softtabstop=0                      " (may be overridden by autocmd rules)
set shiftwidth=4
set expandtab
set shiftround
set autoindent
set smartindent
set cindent
set breakindent
set hidden                             " hidden buffers
set hlsearch                           " searching
set incsearch
set ignorecase
set smartcase
set inccommand=split                   " live substitution (nvim only)
set shell=/bin/zsh                     " default $SHELL
set ruler                              " visual settings
set relativenumber
set listchars=tab:▸\ ,trail:∙,conceal:┊,nbsp:·
set nolist                             " (hidden by default)
set mouse=a                            " mouse support
set mousemodel=popup
set nojoinspaces                       " one space, not two, after punctuation
set splitbelow splitright
set grepprg=rg\ --vimgrep
set t_Co=256                           " termical color model
set conceallevel=2                     " concealing
set foldenable                         " folding
set foldmethod=syntax
set foldlevelstart=99
set foldcolumn=0
set foldtext=foldtext#foldtext()

syntax on

" Leader
let mapleader=','

" Other global options
let g:CSApprox_loaded = 1
let g:indentLine_enabled = 1
let g:indentLine_concealcursor = 0
let g:indentLine_char = '┆'
let g:indentLine_faster = 1

let g:make = 'gmake'                   " for vimproc
if exists('make')
        let g:make = 'make'
endif

" Disable the blinking cursor
set gcr=a:blinkon0
set scrolloff=3

" Modeline and gutter
set laststatus=2
" CVE-2019-12735.
set nomodeline

set title
set titleold="Terminal"
set titlestring=%F

" Git gutter/messenger
let g:gitgutter_map_keys = 0
let g:gitgutter_max_signs = 1000
let g:gitgutter_sign_added = '│'
let g:gitgutter_sign_modified = '│'
let g:gitgutter_sign_removed = '│'
let g:gitgutter_sign_removed_first_line = '│'
let g:gitgutter_sign_modified_removed = '│'
let g:git_messenger_no_default_mappings = v:true
let g:blameLineGitFormat = '   %an | %ar | %s'

" NERDTree
let g:NERDTreeChDirMode=2
let g:NERDTreeIgnore=['\.rbc$', '\~$', '\.pyc$', '\.db$', '\.sqlite$', '__pycache__']
let g:NERDTreeSortOrder=['^__\.py$', '\/$', '*', '\.swp$', '\.bak$', '\~$']
let g:NERDTreeShowBookmarks=1
let g:NERDTreeMapOpenInTabSilent = '<RightMouse>'
let g:NERDTreeWinSize = 40
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.pyc,*.db,*.sqlite

" Prettier
let g:prettier#autoformat = 1
let g:prettier#autoformat_require_pragma = 0
let g:prettier#exec_cmd_async = 1
let g:prettier#quickfix_enabled = 0

" ale
let g:ale_sign_error = '▮'
let g:ale_sign_warning = '▯'
let g:ale_r_lintr_options = get(g:, 'ale_r_lintr_options', 'with_defaults(assignment_linter=NULL,commented_code_linter=NULL,line_length_linter(120))')
let g:ale_linters = {'python': ['flake8', 'mypy'], 'markdown': ['mdl']}
let g:ale_fixers = {'python': ['autopep8', 'yapf'], 'javascript': ['eslint']}
let g:ale_python_mypy_options = '--ignore-missing-imports'
" let g:ale_fix_on_save = 1

" Tagbar
let g:tagbar_autofocus = 1

" Copy/Paste/Cut
if has('unnamedplus')
  set clipboard=unnamed,unnamedplus
endif

" Rust
let g:rust_recommended_style = 0
let g:rustfmt_autosave = 1

" C
autocmd FileType c setlocal tabstop=4 shiftwidth=4 expandtab
autocmd FileType cpp setlocal tabstop=4 shiftwidth=4 expandtab

" markdown
let g:vim_markdown_math = 1
let g:vim_markdown_follow_anchor = 1
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_new_list_item_indent = 2
let g:vim_markdown_folding_level = 2
let g:vim_markdown_emphasis_multiline = 0
let g:vim_markdown_fenced_languages = ['julia=jl', 'python=py']


" Other config files
source ~/.config/nvim/plug.vim
source ~/.config/nvim/mappings.vim
source ~/.config/nvim/lsp.vim
source ~/.config/nvim/cmd.vim

" Theme
colorscheme nord
let g:nord_italic = 1
let g:nord_italic_comments = 1
let g:nord_comment_brightness = 20

" vim-airline
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

let g:airline_theme = 'base16'
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#hunks#enabled = 0
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#show_tab_type = 0
let g:airline#extensions#tabline#tab_min_count = 2
let g:airline#extensions#tagbar#enabled = 0
let g:airline_skip_empty_sections = 1
" let g:airline#extensions#virtualenv#enabled = 1
" let g:airline#extensions#fzf#enabled = 1
" let g:airline#extensions#fugitiveline#enabled = 1
" let g:airline#extensions#grepper#enabled = 1

let g:airline#extensions#tabline#left_sep     = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'
let g:airline_left_alt_sep                    = '»'
let g:airline_right_alt_sep                   = '«'
let g:airline#extensions#branch#prefix        = '⎇'
let g:airline#extensions#readonly#symbol      = '⊘'
let g:airline#extensions#linecolumn#prefix    = '¶'
let g:airline#extensions#paste#symbol         = 'ρ'
let g:airline_symbols.linenr                  = ''
let g:airline_symbols.branch                  = ''
let g:airline_symbols.notexists               = '⤓'
let g:airline_symbols.dirty                   = '⚡'
let g:airline_symbols.paste                   = 'ρ'
let g:airline_symbols.readonly                = ''
let g:airline_symbols.whitespace              = 'Ξ'

au User AirlineAfterInit  :let g:airline_section_z = airline#section#create(['%3p%% %l:%3v'])

highlight Comment cterm=italic  " vim
highlight Comment gui=italic    " nvim

hi GitGutterAdd guibg=none
hi GitGutterChange guibg=none
hi GitGutterChangeDelete guibg=none
hi GitGutterDelete guibg=none
