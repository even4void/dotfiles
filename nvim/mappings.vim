"" Split
noremap <Leader>- :<C-u>split<CR>

"" Buffer nav
noremap <leader>j :bp<CR>
noremap <leader>k :bn<CR>
noremap <leader>c :bd<CR>

nnoremap <silent> <C-d><C-d> :confirm bdelete<CR>

"" Tabs
nnoremap <leader>a :tabnew<CR>
nnoremap <leader>h :tabprevious<CR>
nnoremap <leader>l :tabnext<CR>

"" Switching windows
noremap <C-j> <C-w>j
noremap <C-k> <C-w>k
noremap <C-l> <C-w>l
noremap <C-h> <C-w>h

"" Nerdtree and Tagbar
nnoremap <leader>z :NERDTreeFind<CR>
nnoremap <silent> <C-P> :NERDTreeToggle<CR>
nmap <Leader>@ :Tagbar<CR>

" terminal emulation
nnoremap <silent> <leader>sh :terminal<CR>

"" Set working directory
nnoremap <leader>. :lcd %:p:h<CR>

"" Opens a (tab) edit command with the path of the currently edited file filled in
noremap <Leader>e :e <C-R>=expand("%:p:h") . "/" <CR>
noremap <Leader>te :tabe <C-R>=expand("%:p:h") . "/" <CR>

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
nnoremap <silent> <leader><leader> :noh<cr>

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
noremap <Leader>ga :Gwrite<CR>
noremap <Leader>gb :Gblame<CR>
noremap <Leader>gc :Gcommit<CR>
noremap <Leader>gd :Gvdiff<CR>
noremap <Leader>gh :GitGutterPreviewHunk<CR>
noremap <Leader>gl :ToggleBlameLine<CR>
noremap <Leader>gm :GitMessenger<CR>
noremap <Leader>gp :Gpush<CR>
noremap <Leader>gr :Gremove<CR>
noremap <Leader>gs :Gstatus<CR>
noremap <Leader>gu :Gpull<CR>

"" fzf
nmap ; :Buffers<CR>
nmap <Leader>f :Files<CR>
nmap <Leader>/ :BLines<CR>
nmap <Leader>t :Tags<CR>
nmap <leader>y :History:<CR>

"" grepper
nmap <unique> <Leader>r :GrepperRg<SPACE>

"" Ale
nmap <Leader>= <Plug>(ale_fix)

"" lsp
nmap gc <cmd>lua vim.lsp.buf.declaration()<CR>
nmap gd <cmd>lua vim.lsp.buf.definition()<CR>
nmap gh <cmd>lua vim.lsp.buf.hover()<CR>
" nmap gi <cmd>lua vim.lsp.buf.implementation()<CR>
nmap gD <cmd>lua vim.lsp.buf.references()<CR>
nmap g* <cmd>lua vim.lsp.buf.document_symbol()<CR>
nmap gr <cmd>lua vim.lsp.buf.rename()<CR>
nmap gf <cmd>lua vim.lsp.buf.formatting()<CR>
nmap gs <cmd>lua vim.lsp.buf.signature_help()<CR>
nmap ga <cmd>lua vim.lsp.buf.code_action()<CR>

autocmd CursorHold * lua vim.lsp.util.show_line_diagnostics()

nmap <leader>d :OpenDiagnostic<CR>
nmap <silent> [g :PrevDiagnosticCycle<CR>
nmap <silent> ]g :NextDiagnosticCycle<CR>
