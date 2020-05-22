;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/packages.el

(package! bibtex-utils)
;; (package! nord-theme)
;; (package! ox-hugo)
(package! org-fancy-priorities)
(package! osx-dictionary)
(package! realgud-trepan-ni)
(package! interleave)
;; (package! ivy-bibtex :pin "3cff6bd70")  ;; missing bibtex-completion.el
(package! vc-msg)
(package! guess-language)
(package! all-the-icons-ibuffer)
(package! keypression)
(package! lsp-julia :recipe (:host github :repo "non-jedi/lsp-julia"))
;; (package! lsp-racket :recipe (:host github :repo "mullikine/lsp-racket-el"))

(unpin! (:tools lsp))

;; no thanks
(package! helm :disable t)
