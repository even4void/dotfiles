;;; ~/.config/doom/lisp/faces.el -*- lexical-binding: t; -*-

;; Mostly derived from Nicolas Rougier's work
;; https://github.com/rougier/elegant-emacs
;; with minor adpatation for the doom-nord theme.
(defadvice load-theme (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

(defun set-face (face style)
  "Reset a face and make it inherit style."
  (set-face-attribute face nil
   :foreground 'unspecified :background 'unspecified
   :family     'unspecified :slant      'unspecified
   :weight     'unspecified :height     'unspecified
   :underline  'unspecified :overline   'unspecified
   :box        'unspecified :inherit    style))

(defface face-popout  '((t :foreground "#c2a282"))         "Popout")
(defface face-strong  '((t :weight regular))               "Strong")
(defface face-salient '((t :foreground "#81a1c1"))        "Salient")
(defface face-faded   '((t :foreground "#999999"))          "Faded")
(defface face-subtle  '((t :background "#c2c282"
                           :foreground "#191c25"
                           :distant-foreground "#f0f4fc")) "Subtle")
(defface face-warning '((t :foreground "#ffa07a"))        "Warning")

(defface face-display
  '((t :family "Fira Code" :inherit 'face-faded))  "Display")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'face-display))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'face-display))

(set-face 'header-line-highlight                          'face-faded)
(set-face 'region                                        'face-subtle)
(set-face 'highlight                                     'face-subtle)
(set-face 'bold                                          'face-strong)
;; (set-face 'italic                                         'face-faded)
(set-face 'cursor                                        'face-strong)
(set-face-attribute 'cursor nil
                    :background (face-foreground        'face-strong))
(set-face 'minibuffer-prompt                             'face-strong)
(set-face 'link                                         'face-salient)
(set-face 'fringe                                         'face-faded)
(set-face 'isearch                                       'face-strong)
(set-face 'lazy-highlight                                'face-subtle)
(set-face 'show-paren-match                              'face-subtle)
(set-face 'show-paren-mismatch                           'face-normal) ;; FIXME
(set-face 'shadow                                         'face-faded)
(set-face 'warning                                      'face-warning)

(after! org
  (set-face 'outline-1                                   'face-strong)
  (set-face 'outline-2                                   'face-strong)
  (set-face 'outline-3                                   'face-strong)
  (set-face 'outline-4                                   'face-strong)
  (set-face 'outline-5                                   'face-strong)
  (set-face 'outline-6                                   'face-strong)
  (set-face 'org-todo                                    'face-popout))

;; (set-face 'flyspell-incorrect                         'face-popout)

;; (set-face 'font-lock-comment-face                      'face-faded)
(set-face 'font-lock-doc-face                             'face-faded)
(set-face 'font-lock-string-face                         'face-popout)
(set-face 'font-lock-constant-face                      'face-salient)
(set-face 'font-lock-warning-face                        'face-popout)
(set-face 'font-lock-function-name-face                  'face-strong)
(set-face 'font-lock-variable-name-face                  'face-strong)
(set-face 'font-lock-builtin-face                       'face-salient)
(set-face 'font-lock-type-face                          'face-salient)
(set-face 'font-lock-keyword-face                       'face-salient)

;; Stuff that I don't know how to handle using the above scheme
(custom-set-faces!
  '(highlight-numbers-number :foreground "#bebf8e")
  `(highlight-quoted-symbol :foreground ,(doom-darken "#81a1c1" 0.2))
  '(ess-modifiers-face :foreground unspecified)  ;; better nothing than half the job done
  '(ess-constant-face :foreground "#bebf8e")
  '(mu4e-header-key-face :foreground "#c2a282")  ;; #4c566a
  '(mu4e-highlight-face :foreground "#81a1c1" :weight bold)
  '(markdown-metadata-key-face :foreground "#4c566a")
  '(markdown-header-delimiter-face :foreground unspecified :weight bold)
  '(markdown-list-face :foreground unspecified :weight regular)
  '(markdown-math-face :foreground unspecified :weight bold)
  '(markdown-header-face-1 :inherit 'outline-1 :weight bold :height 1.05)
  '(markdown-header-face-2 :inherit 'outline-2 :weight bold)
  '(markdown-italic-face :foreground unspecified :slant italic)
  '(markdown-link-face :foreground "#81a1c1" :underline (:color "#81a1c1"))
  `(markdown-url-face :foreground ,(doom-darken "#81a1c1" 0.2))
  '(markdown-pre-face :foreground "#bebf8e")
  '(markdown-code-face :background "#f9f9f9")
  '(markdown-inline-code-face :background nil)
  '(dired-directory :foreground "#81a1c1")
  `(dired-symlink :foreground ,(doom-darken "#81a1c1" 0.2))
  '(pdf-isearch-batch :foreground "#bebf8e")
  '(ivy-subdir :foreground "#81a1c1")
  '(flyspell-incorrect :underline (:color "#c2a282"))
  '(flyspell-duplicate :underline (:color "#bebf8e"))
  '(writegood-weasels-face :background nil :underline (:color "#bfa78e" :style wave))
  '(writegood-passive-voice-face :background nil :underline (:color "#81a1c1" :style wave))
  '(writegood-duplicates-face :background nil :underline (:color "#bebf8e" :style wave))
  '(org-document-title :foreground "#c2a282")
  '(org-level-1 :inherit 'outline-1 :weight bold :height 1.05)
  '(org-level-2 :inherit 'outline-2 :weight bold)
  '(org-link :foreground "#81a1c1" :underline (:color "#81a1c1"))
  '(org-footnote :foreground "#999999")
  '(org-verbatim :foreground "#bebf8e")
  '(org-drawer :foreground "#999999")
  ;; '(org-quote :background nil)
  '(org-block-begin-line :background "#f9f9f9")
  '(org-block-end-line :background "#f9f9f9")
  '(org-block :background "#f9f9f9")
  '(org-journal-calendar-entry-face :foreground "#c2a282" :slant normal)
  '(org-journal-calendar-scheduled-face :foreground "#bf616a" :slant normal)
  '(lsp-ui-sideline-code-action :foreground "#ffa07a")
  '(racket-keyword-argument-face :foreground "#c2a282")
  '(racket-selfeval-face :foreground "#c2a282")
  '(diredfl-date-time :foreground "#81a1c1")
  (unless (featurep! "+light")
    '(mode-line :family "Helvetica Neue" :height 0.96)
    '(mode-line-inactive :family "Helvetica Neue" :height 0.96))
  '(doom-modeline-buffer-modified :foreground "#c2a282"))
