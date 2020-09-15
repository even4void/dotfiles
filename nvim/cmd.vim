"" The PC is fast enough, do syntax highlight syncing from start unless 200 lines
augroup vimrc-sync-fromstart
  autocmd!
  autocmd BufEnter * :syntax sync maxlines=200
augroup END

"" Update Emacs timestamp
function! LastModified()
  if &modified
    let save_cursor = getpos(".")
    let n = min([20, line("$")])
    keepjumps exe '1,' . n . 's#^\(.\{,10}Time-stamp: \).*#\1' .
          \ strftime('<%Y-%m-%d %H:%M:%S chl>') . '#e'
    call histdel('search', -1)
    call setpos('.', save_cursor)
  endif
endfun
autocmd BufWritePre * call LastModified()

"" Remember cursor position
augroup vimrc-remember-cursor-position
  autocmd!
  autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"" | endif
augroup END

" Highlight yanked text (neovim 0.5-nightly)
augroup LuaHighlight
  autocmd!
  autocmd TextYankPost * silent! lua require'vim.highlight'.on_yank()
augroup END

"" Whitespace
" autocmd FileType c,cpp,python,markdown,scheme,haskell autocmd BufWritePre <buffer> :%s/\s\+$//e
autocmd BufWritePre * %s/\s\+$//e

"" Spelling
autocmd BufRead,BufNewFile *.md,*.org setlocal spell
autocmd FileType gitcommit setlocal spell

"" Indentation for specific file type (no editorconfig, no ftplugin)
autocmd FileType html setlocal shiftwidth=2 tabstop=2
autocmd FileType javascript setlocal shiftwidth=2 tabstop=2
autocmd FileType css setlocal shiftwidth=2 tabstop=2
autocmd FileType c,cpp setlocal tabstop=4 shiftwidth=4 expandtab

"" make/cmake
augroup vimrc-make-cmake
  autocmd!
  autocmd FileType make setlocal noexpandtab
  autocmd BufNewFile,BufRead CMakeLists.txt setlocal filetype=cmake
augroup END

set autoread
