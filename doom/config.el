;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 126))

(setq user-full-name "chl"
      user-mail-address "chl@aliquote.org"
      epa-file-encrypt-to user-mail-address
      auth-sources '("~/.authinfo.gpg"))

(when (display-graphic-p)
    (setq doom-font (font-spec :family "Iosevka" :size 14)
          doom-variable-pitch-font (font-spec :family "Iosevka" :size 14))
    (load! "+iosevka"))

(load! "+bindings")

;; ---------------------------------------------------------------------------
;; ui
;; ---------------------------------------------------------------------------
(load-theme 'doom-nord t)
(setq which-key-idle-delay 0.1)
(setq ns-use-proxy-icon nil)
(set-face-italic 'font-lock-comment-face t)
(setq mac-option-modifier 'none)
(delete-selection-mode 1)
(after! doom-modeline
  (doom-modeline-def-modeline 'my/modeline
    '(bar matches buffer-info remote-host buffer-position selection-info)
    '(objed-state misc-info debug input-method buffer-encoding major-mode process vcs))
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'my/modeline 'default))
  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline))
(setq doom-themes-neotree-file-icons 'simple)
(setq doom-modeline-env-python-executable "python3"
      doom-modeline-enable-word-count t
      doom-modeline-checker-simple-format t
      doom-modeline-unicode-fallback nil
      doom-modeline-github nil
      doom-modeline-mu4e nil)

;; ---------------------------------------------------------------------------
;; packages
;; ---------------------------------------------------------------------------
(add-hook 'fish-mode-hook
          (lambda () (add-hook 'before-save-hook 'fish_indent-before-save)))

;; -- web & doc --------------------------------------------------------------
(setq browse-url-browser-function 'eww-browse-url)

;; --tex ---------------------------------------------------------------------
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
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

;; -- text/markdown editing --------------------------------------------------
(setq time-stamp-active t
      time-stamp-line-limit 10)
(add-hook 'write-file-functions 'time-stamp)
(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude "^~/org/.export"))
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq +format-on-save-enabled-modes
  '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
        ess-mode         ; FIXME styler needs configuration
        sql-mode         ; sqlformat is currently broken
        python-mode))    ; because I don't like it
(remove-hook 'dired-mode-hook 'diredfl-mode)
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
        markdown-code-face "Inziu Iosevka CL"))

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

;; -- deft -------------------------------------------------------------------
(setq deft-directory "~/org/drafts"
      deft-recursive t)

;; -- company ----------------------------------------------------------------
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 3
        company-show-numbers nil))
        ;; company-tooltip-offset-display nil

;; -- ess --------------------------------------------------------------------
(setq ess-use-eldoc t
      ess-eldoc-show-on-symbol t
      ess-execute-in-process-buffer t)
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
        lsp-ui-doc-enable t
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-include-signature t))

(add-hook 'lsp-ui-doc-frame-hook
          (lambda (frame _w)
            (set-face-attribute 'default frame :font "Iosevka" :height 130)))

;; (setq-default lsp-pyls-configuration-sources ["flake8"])
;; (setq lsp-pyls-plugins-pylint-enabled nil
;;       lsp-pyls-plugins-pyflakes-enabled nil)
;; (setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
(setq ccls-executable "~/local/ccls/Release/ccls")

;; -- lisp/haskell -----------------------------------------------------------
(setq inferior-lisp-program "ccl64")

(add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
(setq geiser-active-implementations '(chez chicken mit))

(after! cider
  (setq cider-eldoc-display-context-dependent-info t))

(after! haskell
  (add-hook 'haskell-mode-hook #'hindent-mode))

(load! "lisp/ra-emacs-lsp")

;; -- org --------------------------------------------------------------------
(setq org-directory "~/org"
      org-babel-clojure-backend 'cider
      inferior-R-program-name "/usr/local/bin/R"
      inferior-STA-program-name "/usr/local/bin/stata-mp")

(after! org
  (setq org-capture-templates
      '(("t" "Personal todo" entry
         (file+headline +org-capture-todo-file "Inbox")
         "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
        ("n" "Personal notes" entry
         (file+headline +org-capture-notes-file "Inbox")
         "* %u %?\n%i\n%a" :prepend t :kill-buffer t)
        ("w" "Web link" entry (file+headline "urls.org" "Inbox")
         "* %? \n%U\n%(retrieve-url)\n" :prepend t)
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
        org-catch-invisible-edits 'error
        org-startup-with-inline-images nil
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-highlight-latex-and-related '(latex)
        org-support-shift-select t
        org-ellipsis " ▼ "
        org-todo-keywords '((sequence "TODO" "STAR" "|" "DONE" "CANC"))
        org-log-done 'time
        org-default-notes-file "~/org/notes.org"
        org-default-todo-file "~/org/todos.org"
        org-bibtex-file "~/org/references.bib"
        org-export-with-author nil
        org-export-with-creator nil
        org-html-postamble nil
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
                                           (template . "/Users/chl/.pandoc/templates/eisvogel.latex" ))))

;; -- mu ---------------------------------------------------------------------
(load! "lisp/mu4e")
