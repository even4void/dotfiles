set background=light

hi clear
if exists("syntax_on")
	syntax reset
endif

let colors_name = "light"

hi Comment		cterm=NONE		    ctermfg=103
hi Constant		cterm=NONE		    ctermfg=131
hi CursorLineNr	cterm=bold		    ctermfg=244
hi DiffAdd		cterm=bold		    ctermfg=NONE
hi DiffChange	cterm=bold		    ctermfg=NONE
hi DiffDelete	cterm=bold		    ctermfg=NONE
hi DiffText		cterm=reverse	    ctermfg=NONE
hi Directory	cterm=bold		    ctermfg=NONE
hi Error		cterm=reverse	    ctermfg=188	    ctermbg=131
hi ErrorMsg		cterm=reverse	    ctermfg=9	    ctermbg=15
hi FoldColumn	cterm=standout	    ctermfg=NONE
hi Folded		cterm=standout	    ctermfg=NONE
hi Identifier	cterm=NONE		    ctermfg=NONE
hi Ignore		cterm=bold		    ctermfg=NONE
hi IncSearch	cterm=reverse	    ctermfg=NONE
hi LineNr		cterm=NONE		    ctermfg=248
hi MatchParen	cterm=bold		    ctermfg=none	ctermbg=185
hi ModeMsg		cterm=bold		    ctermfg=NONE
hi MoreMsg		cterm=bold		    ctermfg=NONE
hi NonText		cterm=bold		    ctermfg=NONE
hi PreProc		cterm=NONE		    ctermfg=NONE
hi Question		cterm=standout	    ctermfg=NONE
hi Search		cterm=reverse	    ctermfg=NONE
hi SignColumn   cterm=NONE          ctermbg=NONE
hi Special		cterm=bold		    ctermfg=NONE
hi SpecialKey	cterm=bold		    ctermfg=NONE
hi Statement	cterm=bold		    ctermfg=NONE
hi StatusLine	cterm=bold,reverse	ctermfg=NONE
hi StatusLineNC	cterm=reverse		ctermfg=NONE
hi TabLine		cterm=reverse		ctermfg=NONE
hi Title		cterm=bold		    ctermfg=NONE
hi Todo			cterm=bold,standout	ctermfg=185	    ctermbg=0
hi Type			cterm=bold		    ctermfg=NONE
hi Underlined	cterm=underline		ctermfg=NONE
hi VertSplit	cterm=reverse		ctermfg=NONE
hi Visual		cterm=NONE		    ctermbg=188
hi VisualNOS	cterm=bold,underline	ctermfg=NONE
hi WarningMsg	cterm=standout		ctermfg=NONE
hi WildMenu		cterm=standout		ctermfg=NONE

hi ColorColumn	cterm=NONE		    ctermfg=NONE	ctermbg=251

" for highlighting stray spaces/tabs (requires match statements in vimrc)
hi ExtraWhitespace	cterm=reverse	ctermfg=185	    ctermbg=NONE

" mostly for nerdtree
hi VertSplit	cterm=bold		    ctermfg=240	    ctermbg=NONE

" Ale
hi ALEError         ctermbg=131 ctermfg=188
hi ALEErrorSign     ctermbg=NONE ctermfg=131
hi ALEWarning       ctermbg=224
hi ALEWarningSign   ctermbg=NONE ctermfg=166
