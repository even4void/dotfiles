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
       modeline
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
       parinfer
       rotate-text
       snippets
       ;; word-wrap

       :emacs
       dired
       electric
       (ibuffer +icons)
       vc
       (undo +tree)

       :term
       eshell
       ;; term
       vterm

       :tools
       biblio
       debugger
       (eval +overlay)
       gist
       (lookup
        +docsets
        +dictionary
        +offline)
       (lsp +peek)
       (magit +forge)
       make
       pdf
       rgb
       ;; taskrunner
       upload

       :lang
       (cc +lsp)
       (clojure +lsp)
       common-lisp
       data
       ;;erlang
       ;;elixir
       ;;elm
       emacs-lisp
       (ess +lsp)
       (haskell +dante)
       (javascript +lsp)
       (latex
        ;; +fold
        +latexmk)
       (markdown +grip)
       ;;(ocaml +opam-site-lisp)
       (org
        +dragndrop
        +gnuplot
        +journal
        +jupyter
        +pandoc
        +present)
       ;;purescript
       (python +lsp +cython +poetry)
       (julia +lsp)
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
       irc

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
   "with_defaults(assignment_linter=NULL,camel_case_linter=NULL,commented_code_linter=NULL,absolute_paths_linter=NULL,line_length_linter(120))"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
