" nvim-lsp specific mappings
nmap ga <cmd>lua vim.lsp.buf.code_action()<CR>
" nmap gc <cmd>lua vim.lsp.buf.declaration()<CR>
nmap gd <cmd>lua vim.lsp.buf.definition()<CR>
nmap gD <cmd>lua vim.lsp.buf.references()<CR>
nmap gf <cmd>lua vim.lsp.buf.formatting()<CR>
nmap gh <cmd>lua vim.lsp.buf.hover()<CR>
" nmap gi <cmd>lua vim.lsp.buf.implementation()<CR>
nmap gr <cmd>lua vim.lsp.buf.rename()<CR>
nmap gs <cmd>lua vim.lsp.buf.signature_help()<CR>
nmap g* <cmd>lua vim.lsp.buf.document_symbol()<CR>

autocmd CursorHold * lua vim.lsp.util.show_line_diagnostics()

nmap <leader>d :OpenDiagnostic<CR>
nmap <silent> [g :PrevDiagnosticCycle<CR>
nmap <silent> ]g :NextDiagnosticCycle<CR>

" Vista (tag bar)
nmap <leader>@ :Vista nvim_lsp<CR>
nmap <leader>& :Vista finder nvim_lsp<CR>
