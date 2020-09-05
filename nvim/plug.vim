call plug#begin('~/.config/nvim/plugged')

"" project/buffers management
Plug 'scrooloose/nerdtree'   " TODO try defx as a replacement or remove altogether
Plug 'airblade/vim-rooter'

"" code editing, text manipulation
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'sheerun/vim-polyglot'
Plug 'vim-scripts/CSApprox'
Plug 'dense-analysis/ale'
Plug 'mbbill/undotree',                    { 'on': 'UndotreeToggle' }
Plug 'prettier/vim-prettier',              { 'do': 'yarn install' }
Plug 'plasticboy/vim-markdown',            { 'for': 'markdown' }
Plug 'mzlogin/vim-markdown-toc',           { 'for': 'markdown' }

"" Git
Plug 'tpope/vim-fugitive'
Plug 'rhysd/git-messenger.vim'
Plug 'airblade/vim-gitgutter'
Plug 'tveskag/nvim-blame-line'

"" theming
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

"" searching tools
Plug 'junegunn/fzf',                       { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

"" lsp stuff
" Plug 'neoclide/coc.nvim',                  {'branch': 'release'}
" Plug 'liuchengxu/vista.vim'  " instead of tagbar

call plug#end()
