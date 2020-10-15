;;; init.el -*- lexical-binding: t; -*-

(setenv "INDOOM" "1")

(doom! :completion
       company
       (ivy +prescient)

       :ui
       deft
       doom
       hl-todo
       modeline
       ophints
       (popup +all +defaults)
       vc-gutter
       workspaces

       :editor
       (evil +everywhere)
       file-templates
       fold
       (format +onsave)
       snippets

       :emacs
       dired
       electric
       vc
       (undo +tree)

       :term
       vterm

       :tools
       biblio
       debugger
       (eval +overlay)
       (lookup
        +docsets
        +dictionary)
       (lsp +peek)
       magit
       make
       rgb

       :lang
       (cc +lsp)
       clojure
       common-lisp
       emacs-lisp
       ess
       haskell
       (javascript +lsp)
       (latex +latexmk)
       markdown
       (org +jupyter +gnuplot)
       (python +lsp +cython +poetry +pyenv)
       (racket +xp)
       (rust +lsp)
       scheme
       sh
       web

       :email
       mu4e

       :checkers
       spell
       syntax

       :app
       irc
       (rss +org)

       :config
       (default +bindings +smartparens))
