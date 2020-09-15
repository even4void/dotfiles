;;; init.el -*- lexical-binding: t; -*-

(setenv "INDOOM" "1")

(doom! :completion
       company
       (ivy +prescient)

       :ui
       deft
       doom
       hl-todo
       (modeline +light)
       nav-flash
       treemacs
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
       ibuffer
       vc
       (undo +tree)

       :term
       vterm

       :tools
       biblio
       (debugger +lsp)
       (eval +overlay)
       gist
       (lookup
        +docsets
        +dictionary)
       (lsp +peek)
       (magit +forge)
       make
       rgb

       :lang
       (cc +lsp)
       clojure
       common-lisp
       elm
       emacs-lisp
       ess
       haskell
       (javascript +lsp)
       (latex +latexmk)
       markdown
       (org +gnuplot +journal +pandoc)
       purescript
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
