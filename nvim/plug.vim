call plug#begin('~/.config/nvim/plugged')

"" project/buffers management
Plug 'tpope/vim-vinegar'
Plug 'airblade/vim-rooter'

"" code editing, text manipulation
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-surround'
Plug 'sheerun/vim-polyglot'
Plug 'dense-analysis/ale'
Plug 'mbbill/undotree',          { 'on': 'UndotreeToggle' }
Plug 'prettier/vim-prettier',    { 'do': 'yarn install' }
Plug 'plasticboy/vim-markdown',  { 'for': 'markdown' }
Plug 'mzlogin/vim-markdown-toc', { 'for': 'markdown' }
Plug 'Raimondi/delimitMate'
Plug 'parsonsmatt/intero-neovim'
Plug 'alx741/vim-hindent'

"" Git
Plug 'tpope/vim-fugitive'
Plug 'rhysd/git-messenger.vim'
Plug 'airblade/vim-gitgutter'

"" theming
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

"" search tools
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'

call plug#end()
