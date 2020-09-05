;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/packages.el

(package! org-fancy-priorities)
(package! vc-msg)
(package! wolfram-mode)
(package! lsp-julia :recipe (:host github :repo "non-jedi/lsp-julia"))
(package! helm :disable t)
(unpin! doom-themes)
