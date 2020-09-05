;;; init.el -*- lexical-binding: t; -*-

(setenv "INDOOM" "1")

(doom! :completion
       company ;; +childframe
       (ivy +prescient) ;; +childframe

       :ui
       deft
       doom
       hl-todo
       ;; indent-guides
       (modeline +light)
       nav-flash
       ;; treemacs
       ophints
       (popup +all +defaults)
       vc-gutter
       ;; window-select
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
       ibuffer ;; +icons
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
       ;; pdf
       rgb

       :lang
       (cc +lsp)
       (clojure +lsp)
       common-lisp
       elm
       emacs-lisp
       (ess +lsp)
       (haskell +dante)
       (javascript +lsp)
       (latex +latexmk)
       markdown
       (org +gnuplot +journal +pandoc)
       purescript
       (python +lsp +cython +poetry +pyenv)
       ;; (julia +lsp)
       (racket +xp)
       (rust +lsp)
       scheme
       sh
       web

       :email
       mu4e

       :checkers
       spell
       ;; grammar
       syntax ;; +childframe

       :app
       irc
       (rss +org)

       :config
       (default +bindings +smartparens))
