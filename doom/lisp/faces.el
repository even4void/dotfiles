;;; ~/.config/doom/lisp/faces.el -*- lexical-binding: t; -*-

;;; mostly derived from Nicolas Rougier's work
;;; https://github.com/rougier/elegant-emacs
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

(defface face-popout  '((t :foreground "#c2a282"))  "Popout")
(defface face-strong  '((t :weight regular))        "Strong")
(defface face-salient '((t :foreground "#81a1c1")) "Salient")
(defface face-faded   '((t :foreground "#999999"))   "Faded")
(defface face-subtle  '((t :background "#f0f0f0"))  "Subtle")
(defface face-warning '((t :foreground "#ffa07a")) "Warning")


(defface face-display
  '((t :family "Fira Code" :inherit 'face-faded))  "Display")
(set-display-table-slot standard-display-table 'truncation
                        (make-glyph-code ?… 'face-display))
(set-display-table-slot standard-display-table 'wrap
                        (make-glyph-code ?↩ 'face-display))

(set-face 'header-line-highlight                          'face-faded)
(set-face 'region                                        'face-subtle)
;; (set-face 'highlight                                     'face-subtle)
(set-face 'bold                                          'face-strong)
(set-face 'italic                                         'face-faded)
(set-face 'cursor                                        'face-strong)
(set-face-attribute 'cursor nil
                    :background (face-foreground        'face-strong))
(set-face 'minibuffer-prompt                             'face-strong)
(set-face 'link                                         'face-salient)
(set-face 'fringe                                         'face-faded)
(set-face 'isearch                                       'face-strong)
(set-face 'lazy-highlight                                'face-subtle)
(set-face 'show-paren-match                              'face-popout)
(set-face 'show-paren-mismatch                           'face-normal)
(set-face 'shadow                                         'face-faded)
(set-face 'warning                                     'face-warning)

(after! org
  (set-face 'outline-1                                     'face-strong)
  (set-face 'outline-2                                     'face-strong)
  (set-face 'outline-3                                     'face-strong)
  (set-face 'outline-4                                     'face-strong)
  (set-face 'outline-5                                     'face-strong)
  (set-face 'outline-6                                     'face-strong)
  (set-face 'org-link                                     'face-salient)
  (set-face 'org-verbatim                                 'face-salient)
  (set-face 'flyspell-incorrect                            'face-popout))

;; (set-face 'font-lock-comment-face                         'face-faded)
(set-face 'font-lock-doc-face                             'face-faded)
(set-face 'font-lock-string-face                         'face-popout)
(set-face 'font-lock-constant-face                      'face-salient)
(set-face 'font-lock-warning-face                        'face-popout)
(set-face 'font-lock-function-name-face                  'face-strong)
(set-face 'font-lock-variable-name-face                  'face-strong)
(set-face 'font-lock-builtin-face                       'face-salient)
(set-face 'font-lock-type-face                          'face-salient)
(set-face 'font-lock-keyword-face                       'face-salient)
;; (set-face 'highlight-numbers-number                      'face-popout)

(custom-set-faces!
  '(highlight-numbers-number :foreground "#bebf8e")
  '(ess-modifiers-face :foreground "#ffffff")  ;; better nothing than half the job done
  '(mu4e-header-key-face :foreground "#4c566a")
  '(markdown-metadata-key-face :foreground "#4c566a")
  '(markdown-header-delimiter-face :foreground "#ffffff" :weight bold)
  '(markdown-header-face-1 :foreground "#ffffff" :weight bold)
  '(markdown-header-face-2 :foreground "#ffffff" :weight bold)
  '(org-level-1 :inherit 'outline-1 :weight bold)
  '(org-level-2 :inherit 'outline-2 :weight bold)
  '(racket-keyword-argument-face :foreground "#c2a282")
  '(racket-selfeval-face :foreground "#c2a282")
  '(ess-constant-face :foreground "#bebf8e"))
