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
(package! mu4e-alert)
(package! minions)

;; (package! all-the-icons-ibuffer)
;; (package! ivy-bibtex :pin "3cff6bd70")  ;; missing bibtex-completion.el
;; (package! lsp-racket :recipe (:host github :repo "mullikine/lsp-racket-el"))

;; pin this in to avoid updating checker icons again
(package! doom-modeline :pin "2b308857677e983ca4eaedc36438ed94aadf9e65")

;; always get the latest version when updating packages
(unpin! (:tools lsp))

;; no thanks
(package! helm :disable t)
