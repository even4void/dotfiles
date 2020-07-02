if exists('g:no_vim_conceal') || !has('conceal') || &enc != 'utf-8'
  finish
endif

syntax match rOperator "<=" conceal cchar=≤
syntax match rOperator ">=" conceal cchar=≥
syntax match rOperator "!=" conceal cchar=≢
syntax match rOperator "%>%" conceal cchar=»

syntax keyword rStatement function conceal cchar=ƒ

hi link rOperator Operator
hi link rStatement Statement
hi! link Conceal Operator

setlocal conceallevel=1
