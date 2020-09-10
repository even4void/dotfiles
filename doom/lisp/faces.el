;;; ~/.config/doom/lisp/faces.el -*- lexical-binding: t; -*-

;; NOTE tints for #bf616a
;; #bf616a #c57078 #cb8087 #d29096 #d8a0a5 #dfb0b4 #e5bfc3 #ebcfd2 #f2dfe1 #f8eff0 #ffffff
;; Also, check if IndianRed (#af5f5f) isn't better for constant-like object and/or 'outstand'

(custom-theme-set-faces! 'doom-plain
  '(default :background nil)
  '(hl-line :background "#f0eee4")
  '(nav-flash-face :foreground nil :background "#e7e7e7" :extend t)
  '(header-line :background "#e7e7e7")
  '(popup-tip-face :background "#e7e7e7")
  '(font-lock-comment-face :foreground "#9099ab")
  '(font-lock-variable-name-face :foreground "#282a2e")
  ;; '(font-lock-constant-face :foreground "#282a2e")
  '(font-lock-function-name-face :foreground "#282a2e" :weight bold)
  '(font-lock-builtin-face :foreground "#000")
  '(font-lock-type-face :foreground "#000")
  '(font-lock-keyword-face :foreground "#282a2e" :weight bold)
  '(highlight-quoted-symbol :foreground "#000")
  '(diredfl-date-time :foreground "#7e7f81")
  '(diredfl-dir-name :weight bold)
  '(git-gutter:deleted :foreground "#bf616a")
  '(org-footnote :foreground "#9099ab")
  '(org-verbatim :foreground "#444" :weight bold)
  '(org-ellipsis :foreground "#c5c8c6")
  '(org-latex-and-related :foreground "#444")
  '(org-journal-calendar-entry-face :background "#9099ab" :foreground "#f0eee4")
  '(markdown-pre-face :foreground "#444")
  '(markdown-inline-code-face :background nil :foreground "#9099ab")
  '(markdown-link-face :foreground "#444" :underline (:color "#444"))
  '(markdown-blockquote-face :foreground "#9099ab")
  '(mu4e-highlight-face :foreground "#444" :weight bold)
  '(magit-header-line :background "#9099ab")
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
