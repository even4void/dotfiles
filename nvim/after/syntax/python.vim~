if exists('g:no_vim_conceal') || !has('conceal') || &enc != 'utf-8'
  finish
endif

syntax match pyOperator "<=" conceal cchar=≤
syntax match pyOperator ">=" conceal cchar=≥
syntax match pyOperator "!=" conceal cchar=≢
syntax keyword pyStatement lambda conceal cchar=λ

" hi link pyOperator Operator
" hi link pyStatement Statement

setlocal conceallevel=1
