;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       (company +childframe)
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

       :term
       eshell
       term

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
    ("f99773b819cbfee5b3b12ec82ee7f9a82a32ebe3637ed60f5c4c062304cb9621" default)))
 '(flycheck-lintr-linters
   "with_defaults(assignment_linter=NULL,camel_case_linter=NULL,commented_code_linter=NULL,absolute_paths_linter=NULL,line_length_linter(120))")
 '(lsp-ui-doc-delay 0.7)
 '(lsp-ui-doc-max-height 8)
 '(lsp-ui-doc-max-width 40)
 '(lsp-ui-flycheck-enable t)
 '(lsp-ui-imenu-colors (quote ("#798cad" "#a2b583")))
 '(lsp-ui-sideline-ignore-duplicate t)
 '(lsp-ui-sideline-show-hover nil)
 '(org-agenda-files
   (quote
    ("~/org/drafts/ngs.org" "/Users/chl/org/micro.org" "/Users/chl/org/notebook.org" "/Users/chl/org/old-notes.org" "/Users/chl/org/papers.org" "/Users/chl/org/quickies.org" "/Users/chl/org/refile.org" "/Users/chl/org/urls.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(lsp-ui-doc-background ((t (:background "#2e3440"))))
 '(lsp-ui-peek-header ((t (:background "slate grey" :foreground "black"))))
 '(lsp-ui-peek-highlight ((t (:background "slate grey" :distant-foreground "white" :foreground "black" :box (:line-width -1 :color "white")))))
 '(lsp-ui-peek-list ((t (:background "#2e3440"))))
 '(lsp-ui-peek-peek ((t (:background "#2e3440"))))
 '(lsp-ui-peek-selection ((t (:background "slate grey" :foreground "black"))))
 '(markdown-code-face ((t (:family "Inziu Iosevka CL"))))
 '(popup-tip-face ((t (:background "slate grey" :foreground "ivory")))))
