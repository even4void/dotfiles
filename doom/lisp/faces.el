;;; ~/.config/doom/lisp/faces.el -*- lexical-binding: t; -*-

;; NOTE tints for #bf616a
;; #bf616a #c57078 #cb8087 #d29096 #d8a0a5 #dfb0b4 #e5bfc3 #ebcfd2 #f2dfe1 #f8eff0 #ffffff
;; Also, check if IndianRed (#af5f5f) isn't better for constant-like object and/or 'outstand'

(custom-theme-set-faces! 'doom-plain
  '(default :background nil)
  '(hl-line :background "#f0eee4")
  '(nav-flash-face :foreground nil :background "#e7e7e7" :extend t)
  '(header-line :background "#e7e7e7")
  '(highlight :background "#e7e7e7")
  '(popup-tip-face :background "#e7e7e7")
  '(font-lock-comment-face :foreground "#9099ab")
  '(font-lock-variable-name-face :foreground "#282a2e")
  '(font-lock-string-face :foreground "#af5f5f")
  ;; '(font-lock-constant-face :foreground "#282a2e")
  '(font-lock-function-name-face :foreground "#282a2e" :weight bold)
  '(font-lock-builtin-face :foreground "#000")
  '(font-lock-type-face :foreground "#000")
  '(font-lock-keyword-face :foreground "#282a2e" :weight bold)
  '(highlight-quoted-symbol :foreground "#000")
  '(highlight-numbers-number :foreground "#af5f5f" :weight normal)
  '(ess-modifiers-face :foreground "#282a2e")
  '(diredfl-date-time :foreground "#7e7f81")
  '(diredfl-dir-name :weight bold)
  '(eww-valid-certificate :foreground "#9099ab" :weight bold)
  '(eww-invalid-certificate :foreground "#bf616a" :weight bold)
  '(info-title-1 :foreground "#282a2e" :weight bold)
  '(info-title-2 :foreground "#444" :weight bold)
  '(git-gutter:deleted :foreground "#bf616a")
  '(org-drawer :inherit 'org-special-keyword)
  '(org-footnote :foreground "#9099ab")
  '(org-verbatim :foreground "#444" :weight bold)
  '(org-ellipsis :foreground "#c5c8c6")
  '(org-latex-and-related :foreground "#444")
  '(org-formula :foreground "#9099ab" :weight bold)
  '(org-journal-calendar-entry-face :background "#9099ab" :foreground "#f0eee4")
  '(org-scheduled-previously :foreground "#c5c8c6")
  '(org-scheduled-today :foreground "#7e7f81")
  '(markdown-pre-face :foreground "#444")
  '(markdown-link-face :foreground "#444" :weight bold :underline t)
  '(markdown-url-face :foreground "#444")
  '(markdown-inline-code-face :foreground "#444" :weight bold)
  '(markdown-math-face :foreground "#444")
  '(markdown-blockquote-face :foreground "#9099ab")
  '(mu4e-highlight-face :foreground "#444" :weight bold)
  '(magit-header-line :background nil)
  '(flycheck-error :foreground "#f0eee4" :background "#bf616a" :underline nil)
  '(flycheck-error-overlay :background "#f2dfe1" :underline nil)
  '(flycheck-warning :background "#f2dfe1" :underline nil)
  '(flycheck-info :background "#fff1aa" :underline nil)
  '(flyspell-duplicate :background nil)
  '(flyspell-incorrect :background "#f2dfe1" :underline nil)
  ;; '(vertical-border :background "#dadada" :foreground "#dadada") ;; HACK
  ;; Kitty light color scheme
  `(vterm-color-black   :background ,(doom-lighten "#20111a" 0.25) :foreground "#20111a")
  `(vterm-color-red     :background ,(doom-lighten "#bd100d" 0.25) :foreground "#bd100d")
  `(vterm-color-green   :background ,(doom-lighten "#858062" 0.25) :foreground "#858062")
  `(vterm-color-yellow  :background ,(doom-lighten "#e9a448" 0.25) :foreground "#e9a448")
  `(vterm-color-blue    :background ,(doom-lighten "#416978" 0.25) :foreground "#416978")
  `(vterm-color-magenta :background ,(doom-lighten "#96522b" 0.25) :foreground "#96522b")
  `(vterm-color-cyan    :background ,(doom-lighten "#98999c" 0.25) :foreground "#98999c")
  `(vterm-color-white   :background ,(doom-lighten "#958b83" 0.25) :foreground "#958b83"))

(set-face-italic 'font-lock-comment-face t)
