;;; ~/.config/doom/lisp/faces.el -*- lexical-binding: t; -*-

;; Some customizations around doom-nord (dark) theme, mostly to be more
;; pleasant with Emacs in CLI mode (use true colors instead of 256-color
;; mode) and to reduce the color burden in prog-mode.
;;
;; TODO Use custom-theme-set-faces! instead and override doom-dark main
;; palette for 256-color mode.

(custom-set-faces!
  '(font-lock-variable-name-face :foreground "white")
  '(font-lock-function-name-face :foreground "#8282c2")
  '(font-lock-keyword-face :foreground "#81a1c1")
  ;; '(font-lock-type-face :foreground "#81a1c1")
  `(font-lock-builtin-face :foreground ,(doom-darken "#ffffff" 0.3))
  `(font-lock-constant-face :foreground ,(doom-darken "#ffffff" 0.3))
  `(ess-constant-face :foreground ,(doom-darken "#ffffff" 0.3))
  '(ess-modifiers-face :foreground "#ffffff")  ;; better nothing than half the job done
  `(highlight-quoted-symbol :foreground ,(doom-darken "#ffffff" 0.3))
  '(highlight-numbers-number :foreground "#bebf8e")
  `(rainbow-delimiters-depth-2-face :foreground ,(doom-darken "#51afef" 0.2))
  `(rainbow-delimiters-depth-3-face :foreground ,(doom-darken "#51afef" 0.4))
  `(rainbow-delimiters-depth-4-face :foreground ,(doom-darken "#51afef" 0.6))
  '(markdown-metadata-key-face :foreground "#bf616a")
  `(markdown-link-face :foreground ,(doom-darken "#51afef" 0.2))
  `(markdown-url-face :foreground ,(doom-darken "#51afef" 0.2))
  `(link :foreground ,(doom-darken "#51afef" 0.2) :weight normal)
  `(org-level-1 :inherit 'outline-1 :foreground ,(doom-darken "#51afef" 0.2))
  `(org-level-2 :inherit 'outline-2 :foreground ,(doom-darken "#51afef" 0.2))
  `(org-level-3 :inherit 'outline-3 :foreground ,(doom-darken "#51afef" 0.2))
  '(org-block :background nil)
  '(org-block-begin-line :background nil)
  '(org-block-end-line :background nil)
  '(org-formula :foreground "#81a1c1")
  '(org-latex-and-related :foreground "#81a1c1")
  '(writegood-weasels-face :background nil :underline (:color "#ebcb8b"))
  '(writegood-duplicates-face :background nil :underline (:color "#ebcb8b"))
  '(flyspell-duplicate :underline (:color "#ebcb8b"))
  )
