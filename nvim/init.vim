" Basic setup
set encoding=utf-8                     " encoding ...
set fileencoding=utf-8
set fileformats=unix,dos,mac
set backspace=indent,eol,start         " fix backspace indent
set tabstop=4                          " indentation rules ...
set softtabstop=0                      " (may be overridden by autocmd rules)
set shiftwidth=4
set expandtab
set shiftround
set autoindent
set smartindent
set cindent
set breakindent
set hidden                             " hidden buffers
set hlsearch                           " searching ...
set incsearch
set ignorecase
set smartcase
set inccommand=split                   " live substitution (nvim only)
set shell=/bin/zsh                     " default $SHELL
set ruler                              " visual settings ...
set number
set listchars=tab:▸\ ,trail:∙,conceal:┊,nbsp:·
set nolist                             " (hidden by default)
set mouse=a                            " mouse support
set mousemodel=popup
set nojoinspaces                       " one space, not two, after punctuation
set splitbelow splitright              " switch to new opened window
set grepprg=rg\ --vimgrep              " FIXME do I really need this?
set t_Co=256                           " termical color model
set conceallevel=2                     " concealing
set foldenable                         " folding ...
set foldmethod=syntax
set foldlevelstart=99
set foldcolumn=0
set foldtext=foldtext#foldtext()

syntax on

" Leader (<Space>)
let mapleader=' '

" Other global options
let g:CSApprox_loaded = 1

" Disable the blinking cursor
" (this is handled by Kitty anyway)
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
let g:prettier#autoformat_require_pragma = 0
let g:prettier#exec_cmd_async = 1
let g:prettier#quickfix_enabled = 0
" let g:prettier#autoformat = 1

" ale
let g:ale_sign_error = '▮'
let g:ale_sign_warning = '▯'
let g:ale_completion_enabled = 1
set omnifunc=ale#completion#OmniFunc
let g:ale_linters = {'python': ['pyls', 'mypy'],
            \ 'cpp': ['clangd'],
            \ 'rust': ['rls'],
            \ 'racket': ['raco']}
let g:ale_fixers = {'*': ['remove_trailing_lines', 'trim_whitespace'],
            \ 'python': ['black'],
            \ 'cpp': ['clangformat'],
            \ 'rust': ['rustfmt'],
            \ 'html': ['prettier'],
            \ 'javascript': ['prettier', 'eslint'],
            \ 'json': ['prettier']}
let g:ale_cpp_clang_executable = '/usr/local/opt/llvm/bin/clang'
let g:ale_cpp_clangd_executable = '/usr/local/opt/llvm/bin/clangd'
let g:ale_cpp_clangd_options = '--background-index -j=4 -log=error'
" let g:ale_rust_rls_executable = '/Users/chl/.local/bin/rust-analyzer'
" let g:ale_rust_rls_toolchain = ''
" let g:ale_rust_rls_config = { 'rust': {
" 		\ 'all_targets': 1,
" 		\ 'build_on_save': 1,
" 		\ 'clippy_preference': 'on' }}
let g:ale_python_mypy_options = '--ignore-missing-imports'
let b:ale_echo_msg_format = '[%linter%] %code: %%s'
let g:ale_fix_on_save = 1
let g:ale_lint_on_save = 1

" Copy/Paste/Cut
if has('unnamedplus')
  set clipboard=unnamed,unnamedplus
endif

" fzf
let g:fzf_layout = { 'down': '20%' }
let g:fzf_preview_window = ''
let g:fzf_buffers_jump = 1
autocmd FileType fzf set laststatus=0 noshowmode noruler
    \| autocmd BufLeave <buffer> set laststatus=2 showmode ruler

" Haskell
let g:hindent_on_save = 1
let g:hindent_command = '/Users/chl/.local/bin/hindent'
let g:intero_start_immediately = 0
let g:intero_use_neomake = 0

" Rust
let g:rust_recommended_style = 0
let g:rustfmt_autosave = 1

" Markdown
let g:vim_markdown_math = 1
let g:vim_markdown_follow_anchor = 1
let g:vim_markdown_frontmatter = 1
let g:vim_markdown_new_list_item_indent = 2
let g:vim_markdown_conceal = 0
let g:vim_markdown_folding_level = 2
let g:vim_markdown_emphasis_multiline = 0
let g:vim_markdown_fenced_languages = ['julia=jl', 'python=py']

" Other config files
source ~/.config/nvim/plug.vim
source ~/.config/nvim/mappings.vim
source ~/.config/nvim/cmd.vim
" source ~/.config/nvim/coc.vim
" source ~/.config/nvim/mappings-coc.vim

" Theme
colorscheme light

" vim-airline
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif

let g:airline_theme = 'minimalist'
let g:airline#extensions#branch#enabled = 1
let g:airline#extensions#hunks#enabled = 0
let g:airline#extensions#ale#enabled = 1
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#show_buffers = 0
let g:airline#extensions#tabline#show_tab_type = 0
let g:airline#extensions#tabline#tab_min_count = 2
let g:airline#extensions#tagbar#enabled = 0
let g:airline_skip_empty_sections = 1
let g:airline#extensions#virtualenv#enabled = 1
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
let g:airline_symbols.dirty                   = '*'
let g:airline_symbols.paste                   = 'ρ'
let g:airline_symbols.readonly                = ''
let g:airline_symbols.whitespace              = 'Ξ'

au User AirlineAfterInit  :let g:airline_section_z = airline#section#create(['%3p%% %l:%3v'])

" kthxbye
hi Comment cterm=italic  " vim
hi Comment gui=italic    " nvim
hi GitGutterAdd guibg=none
hi GitGutterChange guibg=none
hi GitGutterChangeDelete guibg=none
hi GitGutterDelete guibg=none
