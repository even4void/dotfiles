;;; ~/.config/doom/lisp/mu4e.el -*- lexical-binding: t; -*-

;;; Specific settings for ox-latex backend

(eval-after-load "ox-latex"
  '(add-to-list 'org-latex-classes
                '("tufte-handout"
                  "\\documentclass[nobib]{tufte-handout}
                   \\usepackage[style=authoryear-comp,autocite=footnote]{biblatex}
                   % TODO Check whether authortitle-icomp is a better fit
                   % and update '~/org/drafts/setup.el' if this is the case
                   \\addbibresource{/Users/chl/org/references.bib}
                   \\usepackage{booktabs}
                   % little HACK for tabular only environment
                   \\setlength{\\doublerulesep}{\\arrayrulewidth}
                   \\let\\tbl\\tabular
                   \\def\\tabular{\\sffamily\\small\\tbl}
                   \\usepackage{graphicx}
                   \\usepackage{microtype}
                   \\usepackage{hyphenat}
                   \\usepackage{marginfix}
                   \\usepackage{amsmath}
                   \\usepackage{morefloats}
                   \\usepackage{fancyvrb}
                   \\fvset{fontsize=\\normalsize}
                   \\usepackage{xspace}
                   \\usepackage{nicefrac}
                   \\usepackage{units}
                   \\usepackage{soul}
                   \\usepackage{xcolor}
                   \\usepackage{hyperref}
                   \\hypersetup{colorlinks,allcolors=darkgray}
                   \\makeatletter
                   \\patchcmd{\\hyper@link@}
                     {{\\Hy@tempb}{#4}}
                     {{\\Hy@tempb}{\\ul{#4}}}
                     {}{}
                   \\makeatother
                   [NO-DEFAULT-PACKAGES]
                   [EXTRA]"
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
(eval-after-load "ox-latex"
  '(add-to-list 'org-latex-packages-alist
                '("AUTO" "babel" t ("pdflatex"))))
(eval-after-load "ox-latex"
  '(add-to-list 'org-latex-packages-alist
                '("AUTO" "polyglossia" t ("xelatex" "lualatex"))))
(eval-after-load "ox-latex"
  '(add-to-list 'org-latex-packages-alist '("autostyle=true" "csquotes")))


