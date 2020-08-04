;;; ~/.config/doom/lisp/faces.el -*- lexical-binding: t; -*-

(custom-set-faces!
  '(header-line-highlight :foreground "#9099ab")
  '(region :background "#c2c282" :foreground "#191c25" :distant-foreground "#f0f4fc")
  '(highlight :background "#c2c282" :foreground "#191c25" :distant-foreground "#f0f4fc")
  '(bold :weight regular)
  ;; '(italic :foreground "#9099ab")
  '(cursor :weight regular)
  '(minibuffer-prompt :weight regular)
  '(link :foreground "#81a1c1")
  '(fringe :foreground "#9099ab")
  '(isearch :weight regular)
  '(lazy-highlight :background "#c2c282" :foreground "#191c25" :distant-foreground "#f0f4fc")
  '(show-paren-match :background "#c2c282" :foreground "#191c25" :distant-foreground "#f0f4fc")
  '(show-paren-mismatch :foreground "#bf616a")
  '(shadow :foreground "#9099ab")
  '(warning :foreground "#bf616a")

  '(org-todo :foreground "#ebcb8b")
  '(org-document-title :foreground "#81a1c1")
  '(org-level-1 :foreground nil :weight bold :height 1.05)
  '(org-level-2 :foreground nil :weight bold)
  '(org-link :foreground "#81a1c1" :underline (:color "#81a1c1"))
  '(org-footnote :foreground "#9099ab")
  '(org-verbatim :foreground "#a3be8c")
  '(org-drawer :foreground "#9099ab")
  ;; '(org-quote :background nil)
  ;; '(org-block-begin-line :background "#f0f4fc")
  ;; '(org-block-end-line :background "#f0f4fc")
  ;; '(org-block :background "#f0f4fc")
  '(org-journal-calendar-entry-face :foreground "#ebcb8b" :slant normal)
  '(org-journal-calendar-scheduled-face :foreground "#bf616a" :slant normal)

  ;; '(font-lock-comment-face :foreground "#9099ab" :slant italic)
  '(font-lock-doc-face :foreground "#9099ab")
  '(font-lock-string-face :foreground "#ebcb8b")
  '(font-lock-constant-face :foreground "#81a1c1")
  '(font-lock-warning-face :foreground "#d08770")
  '(font-lock-function-name-face :foreground nil :weight regular :slant italic)
  '(font-lock-variable-name-face :foreground nil :weight regular)
  '(font-lock-builtin-face :foreground "#81a1c1")
  '(font-lock-type-face :foreground "#81a1c1")
  '(font-lock-keyword-face :foreground "#81a1c1")

  `(highlight-numbers-number :foreground ,(doom-darken "#ebcb8b" 0.2))
  `(highlight-quoted-symbol :foreground ,(doom-darken "#81a1c1" 0.2))

  '(ess-modifiers-face :foreground unspecified)  ;; better nothing than half the job done
  '(ess-constant-face :foreground "#ebcb8b")

  '(mu4e-header-key-face :foreground "#9099ab")
  '(mu4e-highlight-face :foreground "#81a1c1" :weight bold)

  '(markdown-metadata-key-face :foreground "#9099ab")
  '(markdown-header-delimiter-face :foreground unspecified :weight bold)
  '(markdown-list-face :foreground unspecified :weight regular)
  '(markdown-math-face :foreground unspecified :weight bold)
  '(markdown-header-face-1 :foreground nil :weight bold :height 1.05)
  '(markdown-header-face-2 :foreground nil :weight bold)
  '(markdown-italic-face :foreground unspecified :slant italic)
  '(markdown-link-face :foreground "#81a1c1" :underline (:color "#81a1c1"))
  `(markdown-url-face :foreground ,(doom-darken "#81a1c1" 0.2))
  '(markdown-pre-face :foreground "#ebcb8b")
  '(markdown-inline-code-face :background nil :inherit 'markdown-pre-face)

  '(dired-directory :foreground "#81a1c1")
  `(dired-symlink :foreground ,(doom-darken "#81a1c1" 0.2))
  '(diredfl-date-time :foreground "#81a1c1")

  `(magit-branch-local :foreground ,(doom-darken "#81a1c1" 0.2))

  '(pdf-isearch-batch :foreground "#c2c282")

  '(ivy-subdir :foreground "#81a1c1")

  '(flyspell-incorrect :underline (:color "#bf616a"))
  '(flyspell-duplicate :underline (:color "#d08770"))

  '(lsp-ui-sideline-code-action :foreground "#a3be8c")
  '(lsp-face-highlight-textual :background "#c2c282")
  '(lsp-face-highlight-read :background "#c2c282" :foreground "#191c25" :distant-foreground "#f0f4fc")
  '(lsp-face-highlight-write :background "#c2c282" :foreground "#191c25" :distant-foreground "#f0f4fc")

  '(racket-keyword-argument-face :foreground "#ebcb8b")
  '(racket-selfeval-face :foreground "#ebcb8b")

  '(doom-modeline-buffer-modified :foreground "#ebcb8b"))
