;; -*- no-byte-compile: t; -*-
;;; ~/.config/doom/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)
(package! bibtex-utils)
(package! nord-theme)
(package! ox-hugo)
(package! org-ref)
(package! osx-dictionary)
(package! realgud-trepan-ni)
(package! fish-mode)
(package! fish-completion)

;; no thanks
(package! helm :disable t)
(package! helm-core :disable t)
(package! helm-bibtex :disable t)
(package! helm-dash :ignore t)
