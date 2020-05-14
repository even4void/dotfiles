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

(package! lsp-julia :recipe (:host github :repo "non-jedi/lsp-julia"))

(unpin! (:tools lsp))

;; no thanks
(package! helm :disable t)
