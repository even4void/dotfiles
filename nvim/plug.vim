call plug#begin('~/.config/nvim/plugged')

"" project/buffers management
Plug 'scrooloose/nerdtree'   " TODO try defx as a replacement or remove altogether
Plug 'liuchengxu/vista.vim'  " instead of tagbar
Plug 'airblade/vim-rooter'

"" code editing, text manipulation
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'sheerun/vim-polyglot'
Plug 'vim-scripts/CSApprox'
Plug 'w0rp/ale'
Plug 'mbbill/undotree',                    { 'on': 'UndotreeToggle' }
Plug 'Yggdroot/indentLine'
Plug 'rust-lang/rust.vim'
Plug 'cespare/vim-toml'
Plug 'JuliaEditorSupport/julia-vim'
Plug 'wlangstroth/vim-racket'
Plug 'prettier/vim-prettier',              { 'do': 'yarn install' }
Plug 'plasticboy/vim-markdown',            { 'for': 'markdown' }
Plug 'mzlogin/vim-markdown-toc',           { 'for': 'markdown' }
Plug 'itspriddle/vim-marked',              { 'for': 'markdown' }
Plug 'jceb/vim-orgmode'

"" Git
Plug 'tpope/vim-fugitive'
Plug 'rhysd/git-messenger.vim'
Plug 'airblade/vim-gitgutter'
Plug 'tveskag/nvim-blame-line'

"" theming
Plug 'arcticicestudio/nord-vim'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

"" searching tools
Plug 'junegunn/fzf',                       { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

"" lsp stuff
Plug 'neovim/nvim-lsp'
Plug 'haorenW1025/diagnostic-nvim'
Plug 'haorenW1025/completion-nvim'
Plug 'ncm2/float-preview.nvim'

call plug#end()
