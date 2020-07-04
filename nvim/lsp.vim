lua <<EOF
    local on_attach_vim = function()
      require'completion'.on_attach()
      require'diagnostic'.on_attach()
    end
    local nvim_lsp = require'nvim_lsp'
    local on_attach_vim = function()
        require'diagnostic'.on_attach()
    end
    nvim_lsp.bashls.setup({on_attach=on_attach_vim})
    nvim_lsp.ccls.setup({on_attach=on_attach_vim})
    nvim_lsp.cssls.setup({on_attach=on_attach_vim})
    nvim_lsp.diagnosticls.setup({on_attach=on_attach_vim})
    nvim_lsp.julials.setup({on_attach=on_attach_vim})
    nvim_lsp.html.setup({on_attach=on_attach_vim})
    nvim_lsp.r_language_server.setup({on_attach=on_attach_vim})
    nvim_lsp.rust_analyzer.setup({on_attach=on_attach_vim})
    nvim_lsp.tsserver.setup({on_attach=on_attach_vim})
    nvim_lsp.pyls.setup{
        on_attach=on_attach_vim,
        settings = {
            pyls = {
                executable = {'/Users/chl/Library/Python/3.7/bin/pyls'},
                configurationSources = {
                    pycodestyle,
                    flake8
                }
            }
        }
    }
EOF

"" Diagnostic settings
let g:diagnostic_enable_virtual_text = 1
let g:diagnostic_enable_underline = 1
let g:diagnostic_show_sign = 0
" let g:diagnostic_auto_popup_while_jump = 1

"" Completion
set completeopt=menuone,noinsert,noselect
set shortmess+=c
let g:completion_trigger_keyword_length = 3
let g:completion_timer_cycle = 50
" let g:completion_auto_change_source = 1
" let g:completion_trigger_on_delete = 1

" Use <Tab> and <S-Tab> for popup menu
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

function! s:check_back_space() abort
    let col = col('.') - 1
    return !col || getline('.')[col - 1]  =~ '\s'
endfunction

inoremap <silent><expr> <TAB>
 \ pumvisible() ? "\<C-n>" :
 \ <SID>check_back_space() ? "\<TAB>" :
 \ completion#trigger_completion()

"" ncm2/float-preview.nvim
let g:float_preview#docked = 1
