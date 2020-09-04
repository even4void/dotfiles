;;; ~/.config/doom/lisp/faces.el -*- lexical-binding: t; -*-

;; NOTE Check 'doom-plain' theme (64b033208d3c2eac33d2b284c4a66eb7bee94c37)

;; standout   #ebcb8b
;; highlight  #c2c282
;; link       #81a1c1
;; blur       #9099ab
;; error      #bf616a
;; constant   #ac4426
;; careful    #ffd7d7

(custom-set-faces!
  '(header-line-highlight :foreground "#9099ab")
  '(region :background "#c2c282" :foreground nil :distant-foreground "#f0eee4")
  '(highlight :background "#c2c282" :foreground nil :distant-foreground "#f0eee4")
  '(nav-flash-face :foreground nil :background "#c2c282" :extend t)
  '(hl-line :background nil)
  '(bold :weight regular)
  '(cursor :weight regular)
  '(minibuffer-prompt :weight regular)
  '(link :foreground "#81a1c1")
  '(fringe :foreground "#9099ab")
  '(isearch :weight regular)
  '(lazy-highlight :background "#c2c282" :foreground nil :distant-foreground "#f0eee4")
  '(show-paren-match :background "#c2c282" :foreground nil :distant-foreground "#f0eee4")
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
  '(font-lock-warning-face :foreground "#ffd7d7")
  '(font-lock-function-name-face :foreground nil :weight bold)
  '(font-lock-variable-name-face :foreground nil :weight regular)
  '(font-lock-builtin-face :foreground nil :weight bold)
  '(font-lock-type-face :foreground nil :weight bold)
  '(font-lock-keyword-face :foreground nil :weight bold)

  `(highlight-numbers-number :foreground ,(doom-darken "#ac4426" 0.2))
  `(highlight-quoted-symbol :foreground nil)

  '(ess-modifiers-face :foreground nil)
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

  '(ivy-subdir :foreground "#81a1c1")

  '(flyspell-incorrect :background "#ffd7d7" :underline nil)
  '(flyspell-duplicate :background nil :underline nil)
  '(popup-tip-face :foreground "#bf616a" :background "#f0eee4")
  '(flycheck-error :foreground "#f0eee4" :background "#bf616a" :underline nil)
  '(flycheck-warning :background "#ffd7d7" :underline nil)

  '(lsp-ui-sideline-code-action :foreground "#bf616a")
  '(lsp-face-highlight-textual :background "#c2c282")
  '(lsp-face-highlight-read :background "#9099ab" :foreground nil :distant-foreground "#f0eee4")
  '(lsp-face-highlight-write :background "#9099ab" :foreground nil :distant-foreground "#f0eee4")

  '(racket-keyword-argument-face :foreground "#81a1c1")
  '(racket-selfeval-face :foreground "#81a1c1")
  '(mode-line :foreground nil :background "#81a1c1")
  '(mode-line-inactive :foreground nil :background "#9099ab")
  '(doom-modeline-buffer-modified :foreground "#ebcb8b"))
