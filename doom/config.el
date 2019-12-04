;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

;; FIXME Check for update on this issue
;; https://github.com/hlissner/doom-emacs/issues/2135
(fset 'battery-update #'ignore)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; (add-to-list 'default-frame-alist '(height . 50))
;; (add-to-list 'default-frame-alist '(width . 175))

(setq user-full-name "chl"
      user-mail-address "chl@aliquote.org"
      epa-file-encrypt-to user-mail-address
      auth-sources '("~/.authinfo.gpg"))

;; Use a patched font for GUI mode so that we get Iosevka ligatures
;; that we have free when using iTerm.
(when (display-graphic-p)
    (setq doom-font (font-spec :family "Iosevka" :size 14)
          doom-variable-pitch-font (font-spec :family "Iosevka" :size 14))
    (load! "+iosevka"))

(load! "+bindings")
;; Override default settings in modules/ui/pretty-code since I definitely
;; don't like how <= and >= are rendered. Hence the local patch and further
;; definitions below (see bookmark "pretty-code-section").
;; (load! "+iosevka")

(load! "lisp/ra-emacs-lsp")

;; ---------------------------------------------------------------------------
;; ui
;; ---------------------------------------------------------------------------
;; doom-themes already comes with a custom Nord theme but I don't like it
;; (moreover we must activate a 24-bit mode for the terminal, see ~/.terminfo).
;; So here we go, with the true https://github.com/arcticicestudio/nord-emacs.
;; (setq nord-comment-brightness 15)
;; (setq nord-region-highlight "frost")
;; (setq nord-uniform-mode-lines t)
;; (load-theme 'nord t)
(load-theme 'doom-nord t)
(setq which-key-idle-delay 0.1)
(setq ns-use-proxy-icon nil)
(set-face-italic 'font-lock-comment-face t)
(setq mac-option-modifier 'none)
(delete-selection-mode 1)
; https://lists.gnu.org/archive/html/help-gnu-emacs/2013-01/msg00271.html
; #x2758 for |; 0x78 for │
(set-display-table-slot standard-display-table
                        'vertical-border (make-glyph-code ?│ 'font-lock-comment-face))

;; doom-modeline setup
(after! doom-modeline
  ;; modals: evil state; indent-info: indentation level
  (doom-modeline-def-modeline 'my/modeline
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process github vcs))
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'my/modeline 'default))
  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline))
(setq doom-themes-neotree-file-icons 'simple)
(setq doom-modeline-env-python-executable "python3")
(setq doom-modeline-enable-word-count t)
(setq doom-modeline-checker-simple-format t)
(setq doom-modeline-unicode-fallback nil)
(setq doom-modeline-github t)
(setq doom-modeline-mu4e t)


;; https://emacs.stackexchange.com/a/36373
(define-fringe-bitmap 'flycheck-fringe-bitmap-ball
  (vector #b00000000
          #b00000000
          #b00000000
          #b01111110
          #b01111110
          #b01111110
          #b01111110
          #b01111110
          #b01111110
          #b01111110
          #b01111110
          #b01111110
          #b01111110
          #b01111110
          #b01111110
          #b00000000
          #b00000000))

(flycheck-define-error-level 'error
  :severity 2
  :compilation-level 2
  :overlay-category 'flycheck-error-overlay
  :fringe-bitmap 'flycheck-fringe-bitmap-ball
  :fringe-face 'flycheck-fringe-error
  :error-list-face 'flycheck-error-list-error)
(flycheck-define-error-level 'warning
  :severity 1
  :compilation-level 1
  :overlay-category 'flycheck-warning-overlay
  :fringe-bitmap 'flycheck-fringe-bitmap-ball
  :fringe-face 'flycheck-fringe-warning
  :warning-list-face 'flycheck-warning-list-error)
(flycheck-define-error-level 'info
  :severity 0
  :compilation-level 0
  :overlay-category 'flycheck-info-overlay
  :fringe-bitmap 'flycheck-fringe-bitmap-ball
  :fringe-face 'flycheck-fringe-info
  :info-list-face 'flycheck-info-list-error)

(setq flycheck-indication-mode 'right-fringe)

;; (setq undo-tree-visualizer-timestamps t)
;; (setq undo-tree-visualizer-diff t)

;; (setq whitespace-style '(trailing lines space-before-tab
;;                          indentation space-after-tab))
;; (setq whitespace-line-column 80)

;; ---------------------------------------------------------------------------
;; packages
;; ---------------------------------------------------------------------------

;; Fish stuff
;; (when (and (executable-find "fish")
;;            (require 'fish-completion nil t))
;;   (global-fish-completion-mode))

(add-hook 'fish-mode-hook
          (lambda () (add-hook 'before-save-hook 'fish_indent-before-save)))

;; -- web & doc --------------------------------------------------------------
(setq browse-url-browser-function 'eww-browse-url)
;; NOTE We can still hit `&' to open the page in an external browser
;; this is mainly to read the Hyperspec doc inline. Note, however, that dash-docs
;; already provides the Hyperspec, so we don't really need our local version.

;; --tex ---------------------------------------------------------------------
(setq TeX-auto-save nil
      TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-method 'synctex
      TeX-source-correlate-start-server t)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

;; -- bibtex -----------------------------------------------------------------
(setq bibtex-field-delimiters 'double-quotes
      bibtex-autokey-year-length 4
      bibtex-autokey-name-year-separator "-"
      bibtex-autokey-year-title-separator "-"
      bibtex-autokey-titleword-separator "-"
      bibtex-autokey-titlewords 2
      bibtex-autokey-titlewords-stretch 1
      bibtex-autokey-titleword-length 5)

(after! ivy-bibtex
  (setq bibtex-completion-bibliography '("~/org/references.bib")
        bibtex-completion-library-path '("~/Documents/Papers"
                                         "~/Documents/Papers/_iBooks")
        bibtex-completion-pdf-extension '(".pdf" ".epub")
        bibtex-completion-notes-path "/Users/chl/org/papers.org"
        bibtex-completion-notes-symbol "≣"
        bibtex-completion-pdf-symbol "◉"
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-open-function (lambda (fpath) (call-process
                                                        "open" nil 0 nil
                                                        "-a" "/Applications/Preview.app"
                                                        fpath))
        bibtex-completion-display-formats
        '((t . "${author:30} ${title:*} ${year:4} ${=has-pdf=:1} ${=has-note=:1} ${=type=:7}"))
        bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default))
        bibtex-completion-notes-template-one-file
        (format "* [[/Users/chl/Documents/Papers/${=key=}.pdf][${=key=}]] - ${title}\n :PROPERTIES:\n :Custom_ID: ${=key=}\n :INTERLEAVE_PDF: /Users/chl/Documents/Papers/${=key=}.pdf\n :END:\n"))
  (advice-add 'bibtex-completion-candidates
              :filter-return 'reverse))

(setq reftex-default-bibliography '("~/org/references.bib"))

;; -- flycheck ---------------------------------------------------------------
(after! flycheck
 (setq
  flycheck-check-syntax-automatically '(mode-enabled save idle-change)))

(setq flycheck-python-pycompile-executable "python3"
      flycheck-python-pylint-executable "python3"
      flycheck-python-flake8-executable "python3")
;; FIXME It looks like this doesn't work unless we use custom-set-variables
(setq flycheck-popup-tip-error-prefix "☣")

;; -- text/markdown editing --------------------------------------------------
(setq time-stamp-active t
      time-stamp-line-limit 10)
(eval-after-load 'recentf
  ;; Pandoc aux files
  '(add-to-list 'recentf-exclude "^~/org/.export"))
(setq show-trailing-whitespace t)
(setq +format-on-save-enabled-modes
  '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
        sql-mode         ; sqlformat is currently broken
        python-mode))    ; because I don't like it
(add-hook 'before-save-hook 'delete-trailing-whitespace)
; (setq require-final-newline t)
(remove-hook 'dired-mode-hook 'diredfl-mode)
(add-hook 'write-file-functions 'time-stamp)
(add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.rmd\\'" . markdown-mode))
(setq markdown-open-command "/usr/local/bin/mark"
      markdown-command "/usr/local/bin/multimarkdown"
      markdown-enable-math t
      markdown-fontify-code-blocks-natively t
      markdown-hide-markup t
      markdown-gfm-uppercase-checkbox t
      markdown-header-scaling-values '(1.1 1.0 1.0 1.0 1.0 1.0))
(after! markdown
  (setq markdown-pre-face "Inziu Iosevka CL"
        markdown-code-face "Inziu Iosevka CL")
  ;; (remove-hook 'markdown-mode-hook #'delete-trailing-whitespace)
  (remove-hook 'markdown-mode-hook #'auto-fill-mode))

;; -- pretty-code ------------------------------------------------------------
;; Best with custom Iosevka font. See, e.g., https://is.gd/L67AoR
(when (display-graphic-p)
  (setq +pretty-code-enabled-modes
        '(emacs-lisp-mode org-mode clojure-mode
                          haskell-mode ;; python-mode
                          latex-mode scheme-mode
                          racket-mode ess-r-mode)))
(setq highlight-indent-guides-responsive 'top
      highlight-indent-guides-delay 0)

;; TODO format-all -- customize styler

;; Org and R additional symbols
;; hex code ▷ (9655), ◇ (9671), ▶ (9654), ƒ (402), ⚐
;; See also https://is.gd/RI0K2P
(when (display-graphic-p)
  (setq +pretty-code-iosevka-font-ligatures
        (append +pretty-code-iosevka-font-ligatures
                '(("[ ]" .  "☐")
                  ("[X]" . "☑" )
                  ("[-]" . "❍" )
                  ("%>%" . #Xe175)
                  ("%$%" . #Xe112)
                  ("%<>%" . #Xe114)
                  ("%T>%" . #Xe1b1)
                  ("function" . "ƒ")
                  ("#+BEGIN_EXAMPLE" . "■")
                  ("#+END_EXAMPLE" . "▪")
                  ("#+BEGIN_COMMENT" . "¶")
                  ("#+END_COMMENT" . "▪")
                  ("#+BEGIN_QUOTE" . "“")
                  ("#+END_QUOTE" . "”")
                  ("#+CAPTION:" . "»")
                  ("#+ATTR_LaTeX:" . "»")
                  ("#+ATTR_LATEX:" . "»")
                  ("#+ATTR_HTML:" . "»")
                  ("#+LABEL:" . "»")
                  ;; ("file:" . "⌘")
                  ("<=" . "⩽")
                  (">=" . "⩾")))))

;; https://is.gd/3VuSXj
(defface org-checkbox-done-text
  '((t (:foreground "#5a637b")))
  "Face for the text part of a checked org-mode checkbox.")

;; See also https://emacs.stackexchange.com/a/52390
;; (font-lock-add-keywords 'org-mode
;;                         '(("@[a-z]+.+?[^;,.]+" . font-lock-keyword-face)))
(font-lock-add-keywords 'org-mode
                        '(("\\[@.+?\\]" . font-lock-keyword-face)))

(font-lock-add-keywords 'org-mode
                        '(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
                           1 'org-checkbox-done-text prepend))
                        'append)

;; -- dash-docs/lookup--------------------------------------------------------
(setq dash-docs-enable-debugging nil)
(setq +lookup-open-url-fn #'eww)
(setq counsel-dash-browser-func #'eww)
(setq counsel-dash-min-length 3)
(set-docsets! 'racket-mode "Racket")
(set-docsets! 'elisp-mode "Emacs Lisp")
(set-docsets! 'lisp-mode "Common Lisp")
(set-docsets! 'clojure-mode "Clojure")
(set-docsets! 'haskell-mode "Haskell")
(set-docsets! 'python-mode "Python")
(set-docsets! 'rust-mode "Rust")

;; -- eshell/term -------------------------------------------------------------
;; https://www.masteringemacs.org/article/complete-guide-mastering-eshell
(after! eshell
  (set-eshell-alias!
   "f"   "(other-window 1) && find-file $1"
   "l"   "ls -lh"
   ".."  "cd ../"
   "d"   "dired $1"
   "gl"  "(call-interactively 'magit-log-current)"
   "gs"  "magit-status"
   "gc"  "magit-commit"))
(defun eshell/clear ()
  "Clear Eshell screen (other than the default method)"
   (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer)))

(setq multi-term-program "/bin/zsh")

;; -- git/magit ---------------------------------------------------------------
;; See https://github.com/magit/ghub/issues/81
(after! magit
  (setq magit-revision-show-gravatars nil))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
      ghub-use-workaround-for-emacs-bug nil)
(setq magit-repository-directories '(("~/git" . 1))
      magit-save-repository-buffers nil)
(setq transient-values '((magit-commit "--gpg-sign=152E3E3F7C4CCE44")
                         (magit-rebase "--autosquash" "--gpg-sign=152E3E3F7C4CCE44")
                         (magit-pull "--rebase" "--gpg-sign=152E3E3F7C4CCE44")))
(setq magit-repolist-columns
      '((""      25 magit-repolist-column-ident                  ())
        ("     " 30 magit-repolist-column-version                ((:right-align t)))
        ("⚡"        1 magit-repolist-column-dirty                  ())
        (""        3 magit-repolist-column-branches               ((:right-align t)))
        (""       3 magit-repolist-column-stashes                ((:right-align t)))
        ("⤓"        3 magit-repolist-column-unpulled-from-upstream ((:right-align t)))
        ("⤒"        3 magit-repolist-column-unpushed-to-upstream   ((:right-align t)))
        ("Path"    99 magit-repolist-column-path                   ())))

;; -- neotree ----------------------------------------------------------------
(setq neo-smart-open t
      neo-vc-integration '(face)
      projectile-switch-project-action 'neotree-projectile-action)

;; (setq projectile-git-submodule-command nil)

;; -- deft -------------------------------------------------------------------
(setq deft-extensions '("org" "md" "txt")
      deft-directory "~/org/drafts"
      deft-text-mode 'org-mode
      deft-use-filename-as-title t
      deft-recursive t
      deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))

;; -- company ----------------------------------------------------------------
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 3
        company-require-match 'never
        company-show-numbers nil
        company-tooltip-offset-display nil
        company-tooltip-align-annotations t
        company-global-modes '(not comint-mode erc-mode message-mode
                                   help-mode gud-mode org-mode text-mode
                                   markdown-mode)))

;; -- ess --------------------------------------------------------------------
(setq ess-use-eldoc t
      ess-eldoc-show-on-symbol t
      ess-execute-in-process-buffer t
      ess-history-file nil)
(add-hook 'inferior-ess-mode-hook 'my/comint-mode-hook)

;; -- python -----------------------------------------------------------------
(setq python-shell-interpreter "python3")
(setq python-shell-interpreter-args "-m IPython --simple-prompt -i")
(setq flycheck-python-pycompile-executable "python3"
      flycheck-python-pylint-executable "python3"
      flycheck-python-flake8-executable "python3")

;; -- lsp --------------------------------------------------------------------
(after! lsp-ui
  (setq lsp-ui-flycheck-enable t
        ;; lsp-ui-flycheck-list-position 'right
        lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-width 80
        lsp-ui-doc-max-height 30))

(add-hook 'lsp-ui-doc-frame-hook
          (lambda (frame _w)
            (set-face-attribute 'default frame :font "Iosevka" :height 130)))

;; For whatever reason, I have to activate flake8 manually; otherwise we end up with
  ;; pyflakes! And the following doesn't seem to work either.
  ;; (setq lsp-clients-python-settings '(:configurationSources ["flake8"]))
(setq-default lsp-pyls-configuration-sources ["flake8"])
(setq lsp-pyls-plugins-pylint-enabled nil
      lsp-pyls-plugins-pyflakes-enabled nil)

;; FIXME At this point, I'm not sure if we really need clangd since Doom relies on ccls
;; when +lsp.
(setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
(setq ccls-executable "~/local/ccls/Release/ccls")

;; -- lisp -------------------------------------------------------------------
(setq inferior-lisp-program "ccl64")

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
(setq geiser-active-implementations '(chez chicken mit))

(after! cider
  (setq cider-eldoc-display-context-dependent-info t))

(after! haskell
  (add-hook 'haskell-mode-hook #'hindent-mode))

;; -- org --------------------------------------------------------------------
(setq org-directory "~/org"
      org-babel-clojure-backend 'cider
      inferior-R-program-name "/usr/local/bin/R"
      inferior-STA-program-name "/usr/local/bin/stata-mp")

(setq org-hugo-default-section-directory "micro"
      org-hugo-default-base-dir "~/Sites/aliquote")

(after! org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (python . t)
     (C . t)
     (R . t)
     (stata . t)
     (lisp . t)
     (emacs-lisp . t)))

  (setq org-capture-templates
      '(("t" "Personal todo" entry
         (file+headline +org-capture-todo-file "Inbox")
         "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
        ("n" "Personal notes" entry
         (file+headline +org-capture-notes-file "Inbox")
         "* %u %?\n%i\n%a" :prepend t :kill-buffer t)
        ("w" "Web link" entry (file+headline "urls.org" "Inbox")
         "* %? \n%U\n%(retrieve-url)\n" :prepend t)
        ("b" "Blog" entry (file+headline "micro.org" "Micro")
         "** TODO %?\n:PROPERTIES:\n:EXPORT_FILE_NAME:\n:END:\n%^g\n" :empty-lines 1)
        ("p" "Templates for projects")
        ("pt" "Project todo" entry  ; {project-root}/todo.org
         (file+headline +org-capture-project-todo-file "Inbox")
         "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
        ("pn" "Project notes" entry  ; {project-root}/notes.org
         (file+headline +org-capture-project-notes-file "Inbox")
         "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
        ("pc" "Project changelog" entry  ; {project-root}/changelog.org
         (file+headline +org-capture-project-notes-file "Unreleased")
         "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)))

  (setq org-hide-emphasis-markers t
        org-tags-column 79
        org-startup-indented nil
        ;; org-indent-indentation-per-level 0
        org-catch-invisible-edits 'error
        org-highlight-links '(bracket angle plain radio tag date footnote)
        org-startup-with-inline-images nil
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-highlight-latex-and-related '(latex)
        org-support-shift-select t
        ;; org-src-tab-acts-natively nil
        ;; org-bullets-bullet-list '("#")
        org-ellipsis " ▼ "
        org-todo-keywords '((sequence "TODO" "STAR" "|" "DONE" "CANC"))
        org-log-done 'time
        org-default-notes-file "~/org/notes.org"
        org-default-todo-file "~/org/todos.org"
        org-bibtex-file "~/org/references.bib"
        org-export-with-author nil
        org-export-with-creator nil
        org-html-postamble nil
        ;; default CSS file in case we don't want pandoc export
        org-html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"_assets/github.css\" />"
        org-latex-pdf-process '("latexmk -pdf -f -outdir=%o %f")
        org-pandoc-options-for-html5 '((section-divs . t)
                                       (bibliography . "/Users/chl/org/references.bib")
                                       ;; https://is.gd/lt21EQ
                                       (template . "/Users/chl/.pandoc/templates/GitHub.html5"))
        org-pandoc-options-for-latex-pdf '((pdf-engine . "lualatex")
                                           (listings . t)
                                           ;; (biblatex . t)
                                           (bibliography . "/Users/chl/org/references.bib")
                                           (template . "/Users/chl/.pandoc/templates/eisvogel.latex" )))
  (remove-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'visual-line-mode))
(remove-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'org-mode-hook #'visual-line-mode)
;; (setq org-pandoc-options '((standalone . t)
;;                            (mathjax . t)
;;                            (smart . t)
;;                            (parse-raw . t)))

;; -- mu ---------------------------------------------------------------------
(load! "lisp/mu4e")

;; (setq dna-do-setup-on-load t)
;; (load! "lisp/dna-mode")
