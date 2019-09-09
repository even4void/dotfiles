;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (company +childframe +tng)
       (ivy +prescient)

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
       (pretty-code +iosevka)
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

       :term
       eshell

       :tools
       debugger
       eval
       (flycheck +childframe)
       flyspell
       gist
       (lookup +docsets)
       lsp
       magit
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
       (haskell +intero)
       (javascript +lsp)
       latex
       markdown
       (ocaml +opam-site-lisp)
       (org
        +dragndrop
        +gnuplot
        +ipython
        +pandoc
        +present)
       ;;purescript
       (python +pyenv +lsp)
       racket
       (rust +lsp)
       scheme
       sh
       web

       :email
       mu4e

       :app

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
    ("f99773b819cbfee5b3b12ec82ee7f9a82a32ebe3637ed60f5c4c062304cb9621" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(markdown-code-face ((t (:family "Inziu Iosevka CL"))))
 '(popup-tip-face ((t (:background "slate grey" :foreground "ivory")))))
