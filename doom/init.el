;;; init.el -*- lexical-binding: t; -*-

(setenv "INDOOM" "1")

(doom! :completion
       (company +childframe)
       (ivy +prescient +childframe)

       :ui
       deft
       doom
       hl-todo
       indent-guides
       (modeline +light)
       minimap
       nav-flash
       treemacs
       ophints
       (popup +all +defaults)
       (pretty-code +iosevka)
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
       (ibuffer +icons)
       vc
       (undo +tree)

       :term
       eshell
       ;; term
       vterm

       :tools
       biblio
       (debugger +lsp)
       docker
       (eval +overlay)
       gist
       (lookup
        +docsets
        +dictionary
        +offline
        +xwidget)
       (lsp +peek)
       (magit +forge)
       make
       pdf
       rgb
       taskrunner
       upload

       :lang
       (cc +lsp)
       (clojure +lsp)
       common-lisp
       data
       elm
       emacs-lisp
       (ess +lsp)
       (haskell +dante)
       (javascript +lsp)
       (latex +latexmk)
       (markdown +grip)
       (org
        +dragndrop
        +gnuplot
        +journal
        +jupyter
        +pandoc
        +present)
       purescript
       (python +lsp +cython +poetry +pyenv)
       (julia +lsp)
       (racket +xp)
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
       irc

       :collab

       :config
       (default +bindings +smartparens))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
