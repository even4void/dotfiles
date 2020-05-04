;;; init.el -*- lexical-binding: t; -*-

(setenv "INDOOM" "1")

(doom! :completion
       (company +childframe)
       (ivy +prescient +childframe)

       :ui
       deft
       doom
       ;;fill-column
       hl-todo
       indent-guides
       (modeline +light)
       nav-flash
       neotree
       ophints
       (popup +all +defaults)
       pretty-code
       ;;treemacs
       vc-gutter
       window-select
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       rotate-text
       snippets

       :emacs
       dired
       electric
       vc
       (undo +tree)

       :term
       eshell
       term
       vterm

       :tools
       biblio
       debugger
       (eval +overlay)
       gist
       (lookup
        +docsets
        +dictionary) ;;+offline
       (lsp +peek)
       (magit +forge)
       make
       pdf

       :lang
       (cc +lsp)
       clojure
       common-lisp
       data
       ;;erlang
       ;;elixir
       ;;elm
       emacs-lisp
       ess
       (haskell +dante)
       (javascript +lsp)
       (latex
        +latexmk
        +fold)
       (markdown +grip)
       ;;(ocaml +opam-site-lisp)
       (org
        +dragndrop
        +gnuplot
        ;;+journal
        +jupyter
        +pandoc
        +present)
       ;;purescript
       (python +lsp +cython +poetry)
       ;; (ruby +lsp)
       racket
       (rust +lsp)
       scheme
       sh
       web

       :email
       mu4e

       :checkers
       spell
       grammar
       (syntax +childframe)

       :app
       ;; (rss +org)

       :collab

       :config
       (default +bindings +smartparens))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f99773b819cbfee5b3b12ec82ee7f9a82a32ebe3637ed60f5c4c062304cb9621" default)))
 '(flycheck-lintr-linters
   "with_defaults(assignment_linter=NULL,camel_case_linter=NULL,commented_code_linter=NULL,absolute_paths_linter=NULL,line_length_linter(120))")
 ;; '(ansi-color-names-vector
 ;;   ["#3B4252" "#BF616A" "#A3BE8C" "#EBCB8B" "#81a1c1" "#B48EAD" "#5E81AC" "gray90"])
 ;; '(lsp-ui-imenu-colors (quote ("#798cad" "#a2b583")))
 '(org-agenda-files
   (quote
    ("~/org/drafts/ngs.org" "/Users/chl/org/micro.org" "/Users/chl/org/notebook.org" "/Users/chl/org/old-notes.org" "/Users/chl/org/papers.org" "/Users/chl/org/quickies.org" "/Users/chl/org/refile.org" "/Users/chl/org/urls.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(lsp-ui-doc-background ((t (:background "#2e3440"))))
 ;;'(lsp-ui-peek-header ((t (:background "slate grey" :foreground "black"))))
 ;;'(lsp-ui-peek-highlight ((t (:background "slate grey" :distant-foreground "white" :foreground "black" :box (:line-width -1 :color "white")))))
 ;;'(lsp-ui-peek-list ((t (:background "#2e3440"))))
 ;;'(lsp-ui-peek-peek ((t (:background "#2e3440"))))
 ;;'(lsp-ui-peek-selection ((t (:background "slate grey" :foreground "black"))))
 '(markdown-code-face ((t (:family "Inziu Iosevka CL"))))
 ;;'(popup-tip-face ((t (:background "slate grey" :foreground "ivory"))))
 )
