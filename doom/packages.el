;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/packages.el

(package! bibtex-utils)
(package! org-fancy-priorities)
(package! osx-dictionary)
(package! realgud-trepan-ni)
(package! interleave)
(package! vc-msg)
(package! guess-language)
(package! keypression)
(package! lsp-julia :recipe (:host github :repo "non-jedi/lsp-julia"))
(package! nord-theme)

;; (package! minions)
;; (package! nordless-theme)
;; (package! mu4e-alert)
;; (package! ox-gfm)
;; (package! ox-leanpub)
;; (package! all-the-icons-ibuffer)
;; (package! ivy-bibtex :pin "3cff6bd70")  ;; missing bibtex-completion.el
;; (package! lsp-racket :recipe (:host github :repo "mullikine/lsp-racket-el"))

;; no thanks
(package! helm :disable t)
