"" Split
noremap <leader>- :<C-u>split<CR>

"" Buffer nav
noremap <leader>j :bp<CR>
noremap <leader>k :bn<CR>
noremap <leader>c :bd<CR>

nnoremap <silent> <C-d><C-d> :confirm bdelete<CR>

"" Tabs
nnoremap <leader>a :tabnew<CR>
nnoremap <leader>h :tabprevious<CR>
nnoremap <leader>l :tabnext<CR>

"" Nerdtree and Tagbar
nnoremap <leader>z :NERDTreeFind<CR>
nnoremap <silent> <C-P> :NERDTreeToggle<CR>

" terminal emulation
nnoremap <silent> <leader>sh :terminal<CR>

"" Set working directory
nnoremap <leader>. :lcd %:p:h<CR>

"" Opens a (tab) edit command with the path of the currently edited file filled in
noremap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
noremap <leader>te :tabe <C-R>=expand("%:p:h") . "/" <CR>

noremap YY "+y<CR>
noremap <leader>p "+gP<CR>
noremap XX "+x<CR>

if has('macunix')
  vmap <C-x> :!pbcopy<CR>
  vmap <C-c> :w !pbcopy<CR><CR>
endif

"" Shift select
imap <S-Left> <ESC>v<Left>
imap <S-Right> <ESC>v<Right>
imap <S-Up> <ESC>v<Up>
imap <S-Down> <ESC>v<Down>

"" Clean search (highlight)
nnoremap <silent> <ESC><ESC> :noh<cr>

"" Vmap for maintain Visual Mode after shifting > and <
vmap < <gv
vmap > >gv

"" Move visual block
vnoremap J :m '>+1<CR>gv=gv
vnoremap K :m '<-2<CR>gv=gv

"" Emacs-like move in INS mode
inoremap <C-e> <C-o>$
inoremap <C-a> <C-o>0

"" Git
noremap <leader>ga :Gwrite<CR>
noremap <leader>gb :Gblame<CR>
noremap <leader>gc :Gcommit<CR>
noremap <leader>gd :Gvdiff<CR>
noremap <leader>gh :GitGutterPreviewHunk<CR>
noremap <leader>gl :ToggleBlameLine<CR>
noremap <leader>gm :GitMessenger<CR>
noremap <leader>gp :Gpush<CR>
noremap <leader>gr :Gremove<CR>
noremap <leader>gs :Gstatus<CR>
noremap <leader>gu :Gpull<CR>

"" fzf
nmap <leader>, :Buffers<CR>
nmap <leader>f :Files<CR>
nmap <leader>/ :BLines<CR>
nmap <leader>t :Tags<CR>
nmap <leader>y :History:<CR>
nmap <leader>r :Rg<SPACE>

"" Ale
nmap <leader>= <Plug>(ale_fix)
