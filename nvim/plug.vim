call plug#begin('~/.config/nvim/plugged')

Plug 'scrooloose/nerdtree'
Plug 'jistr/vim-nerdtree-tabs'
" Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'majutsushi/tagbar'
Plug 'justinmk/vim-dirvish'
Plug 'airblade/vim-rooter'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-rhubarb'
Plug 'rhysd/git-messenger.vim'
Plug 'airblade/vim-gitgutter'
Plug 'tveskag/nvim-blame-line'
" Plug 'mhinz/vim-signify'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-scripts/grep.vim'
Plug 'mhinz/vim-grepper',                  { 'on': ['Grepper', 'GrepperRg', '<Plug>(GrepperOperator)'] }
Plug 'sheerun/vim-polyglot'
Plug 'vim-scripts/CSApprox'
" Plug 'vim-scripts/utl.vim'
" Plug 'Raimondi/delimitMate'
Plug 'w0rp/ale'
Plug 'mbbill/undotree',                    { 'on': 'UndotreeToggle' }
Plug 'Yggdroot/indentLine'
Plug 'machakann/vim-highlightedyank'
" Plug 'neoclide/coc.nvim',                  { 'branch': 'release' }
Plug 'prettier/vim-prettier',              { 'do': 'yarn install' }
Plug 'Shougo/vimproc.vim',                 { 'do': g:make }
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'
Plug 'arcticicestudio/nord-vim'
Plug 'vim-scripts/c.vim',                  { 'for': ['c', 'cpp'] }
Plug 'ludwig/split-manpage.vim'
Plug 'rust-lang/rust.vim'
Plug 'cespare/vim-toml'
Plug 'wlangstroth/vim-racket'
Plug 'zizhongyan/vim-stata'
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown',            { 'for': 'markdown' }
Plug 'mzlogin/vim-markdown-toc',           { 'for': 'markdown' }
Plug 'itspriddle/vim-marked',              { 'for': 'markdown' }
" Plug 'jceb/vim-orgmode'
Plug 'junegunn/fzf',                       { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

call plug#end()
