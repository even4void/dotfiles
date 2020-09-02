;;; ~/.config/doom/lisp/faces.el -*- lexical-binding: t; -*-

(custom-set-faces!
  '(header-line-highlight :foreground "#9099ab")
  '(region :background "#c2c282" :foreground "#191c25" :distant-foreground "#f0f4fc")
  '(highlight :background "#c2c282" :foreground "#191c25" :distant-foreground "#f0f4fc")
  '(hl-line :background nil)
  '(bold :weight regular)
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
  '(org-verbatim :foreground "#ac4426")
  '(org-code :foreground "#ebcb8b")
  '(org-drawer :foreground "#9099ab")
  '(org-journal-calendar-entry-face :foreground "#ebcb8b" :slant normal)
  '(org-journal-calendar-scheduled-face :foreground "#bf616a" :slant normal)

  '(font-lock-doc-face :foreground nil)
  '(font-lock-comment-face :foreground "#9099ab")
  '(font-lock-string-face :foreground "#ac4426")
  '(font-lock-constant-face :foreground nil)
  '(font-lock-warning-face :foreground "#bf616a")
  '(font-lock-function-name-face :foreground nil :weight bold)
  '(font-lock-variable-name-face :foreground nil :weight regular)
  '(font-lock-builtin-face :foreground nil :weight bold)
  '(font-lock-type-face :foreground nil :weight bold)
  '(font-lock-keyword-face :foreground nil :weight bold)

  `(highlight-numbers-number :foreground ,(doom-darken "#ebcb8b" 0.2))
  `(highlight-quoted-symbol :foreground nil)

  '(ess-modifiers-face :foreground nil)  ;; better nothing than half the job done
  '(ess-constant-face :foreground "#ac4426")

  '(mu4e-header-key-face :foreground "#9099ab")
  '(mu4e-highlight-face :foreground "#81a1c1" :weight bold)

  '(markdown-metadata-key-face :foreground "#9099ab")
  '(markdown-header-face :foreground nil :weight bold)
  '(markdown-header-delimiter-face :foreground nil :weight bold)
  '(markdown-bold-face :foreground nil :weight bold)
  '(markdown-list-face :foreground unspecified :weight regular)
  '(markdown-math-face :foreground unspecified :weight bold)
  '(markdown-header-face-1 :foreground nil :weight bold :height 1.05)
  '(markdown-header-face-2 :foreground nil :weight bold)
  '(markdown-italic-face :foreground unspecified :slant italic)
  '(markdown-link-face :foreground "#81a1c1" :underline (:color "#81a1c1"))
  `(markdown-url-face :foreground ,(doom-darken "#81a1c1" 0.2))
  '(markdown-pre-face :foreground "#ac4426")
  '(markdown-inline-code-face :background nil :inherit 'markdown-pre-face)

  '(dired-directory :foreground "#81a1c1")
  `(dired-symlink :foreground ,(doom-darken "#81a1c1" 0.2))
  '(diredfl-date-time :foreground "#9099ab")

  `(magit-branch-local :foreground ,(doom-darken "#81a1c1" 0.2))

  '(pdf-isearch-batch :foreground "#c2c282")

  '(ivy-subdir :foreground "#81a1c1")

  '(flyspell-incorrect :underline (:color "#bf616a"))
  '(flyspell-duplicate :underline (:color "#d08770"))

  '(writegood-weasels-face :background nil :underline (:color "#d08770" :style wave))
  '(writegood-passive-voice-face :background nil :underline (:color "#d08770" :style wave))
  '(writegood-duplicates-face :background nil :underline (:color "#d08770" :style wave))

  '(lsp-ui-sideline-code-action :foreground "#ac4426")
  '(lsp-face-highlight-textual :background "#c2c282")
  '(lsp-face-highlight-read :background "#9099ab" :foreground "#191c25" :distant-foreground "#f0f4fc")
  ;; '(lsp-face-highlight-write :background "#9099ab" :foreground "#191c25" :distant-foreground "#f0f4fc")

  '(racket-keyword-argument-face :foreground "#81a1c1")
  '(racket-selfeval-face :foreground "#81a1c1")
  '(mode-line :background "#9099ab")
  `(mode-line-inactive :foreground "#ccc" :background ,(doom-darken "#9099ab" 0.2))
  '(doom-modeline-buffer-modified :foreground "#ebcb8b"))
