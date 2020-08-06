;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

;; (add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(top . 12))
(add-to-list 'default-frame-alist '(left . 12))
(add-to-list 'default-frame-alist '(width . 148))
(add-to-list 'default-frame-alist '(height . 47))
(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
(setq frame-resize-pixelwise t)

(setq default-directory "~/")

(setq user-full-name "chl"
      user-mail-address "chl@aliquote.org"
      epa-file-encrypt-to user-mail-address
      auth-sources '("~/.authinfo.gpg"))

(when (display-graphic-p)
  (setq doom-font (font-spec :family "JetBrains Mono" :size 13)
        doom-big-font (font-spec :family "Victor Mono" :size 16)
        doom-variable-pitch-font (font-spec :family "Helvetica Neue" :size 12)))

(unless (display-graphic-p)
  (custom-set-variables
   '(git-gutter:modified-sign "│")
   '(git-gutter:added-sign "│")
   '(git-gutter:deleted-sign "│"))
  (setq org-superstar-leading-fallback ?\s)
  (setf mac-command-modifier 'super)
  (remove-hook 'text-mode-hook 'highlight-indent-guides-mode)
  (remove-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(load! "+bindings")
(load! "lisp/+light-fix")
;; (load! "lisp/tab-line-custom")

;; -- ui ---------------------------------------------------------------------
(load-theme 'doom-nord t)
(load! "lisp/faces")
(setq doom-themes-enable-bold nil)
(set-face-italic 'font-lock-comment-face t)

(setq which-key-idle-delay 0.2)
(setq ns-use-proxy-icon nil)
(setq mac-option-modifier 'none)
(delete-selection-mode 1)
(setq display-line-numbers-type 'relative)

(when (display-graphic-p)
  (setq highlight-indent-guides-responsive 'top
        highlight-indent-guides-delay 0))

(font-lock-add-keywords 'org-mode
                        '(("\\[@.+?\\]" . font-lock-keyword-face)))

;; -- web & doc --------------------------------------------------------------
(setq browse-url-browser-function 'xwidget-webkit-browse-url)

;; -- tex --------------------------------------------------------------------
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-to-list 'auto-mode-alist '("\\.rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))

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
        bibtex-completion-library-path '("~/Documents/Papers")
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
        '((t . "${author:30} ${title:60} ${year:4} ${=has-pdf=:1} ${=has-note=:1} ${=type=:7}"))
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
 (setq flycheck-check-syntax-automatically '(mode-enabled save idle-change)))

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

(when (display-graphic-p)
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
    :info-list-face 'flycheck-info-list-error))

(setq flycheck-indication-mode 'right-fringe)

(after! git-gutter-fringe
  (when +vc-gutter-default-style
    (if (fboundp 'fringe-mode) (fringe-mode '4))
    (setq-default fringes-outside-margins t)
    (define-fringe-bitmap 'git-gutter-fr:added [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:modified [224]
      nil nil '(center repeated))
    (define-fringe-bitmap 'git-gutter-fr:deleted [224]
      nil nil '(center repeated))))

;; -- Spelling ---------------------------------------------------------------
;; (setq ispell-dictionary "en")

;; -- text/markdown editing --------------------------------------------------
(setq time-stamp-active t
      time-stamp-line-limit 10)
(add-hook 'write-file-functions 'time-stamp)
(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude "^~/org/.export"))
(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude "^~/Sites/aliquote/content/micro"))
(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude "^~/.mail"))
(eval-after-load 'recentf
  '(add-to-list 'recentf-exclude "^~/.emacs.d/.local/etc"))
(setq show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq +format-on-save-enabled-modes
      '(not emacs-lisp-mode
            lisp-mode
            ess-r-mode))

;; (remove-hook 'dired-mode-hook 'diredfl-mode)
(remove-hook 'text-mode-hook #'auto-fill-mode)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)

(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(setq markdown-open-command "/usr/local/bin/mark"
      markdown-command "/usr/local/bin/multimarkdown"
      markdown-enable-math t
      markdown-fontify-code-blocks-natively t
      markdown-hide-markup t
      markdown-gfm-uppercase-checkbox t
      markdown-header-scaling-values '(1.1 1.0 1.0 1.0 1.0 1.0))
(after! markdown
  (setq markdown-pre-face "JetBrains Mono"
        markdown-code-face "JetBrains Mono"))

;; -- dash-docs/lookup--------------------------------------------------------
(setq dash-docs-enable-debugging nil)
(setq +lookup-open-url-fn #'+lookup-xwidget-webkit-open-url-fn)
(setq counsel-dash-browser-func #'+lookup-xwidget-webkit-open-url-fn)
(setq counsel-dash-min-length 3)

;; -- eshell/term -------------------------------------------------------------
(setq vterm-shell "/bin/zsh")

;; -- git/magit ---------------------------------------------------------------
(after! magit
  (setq magit-revision-show-gravatars nil))
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
      ghub-use-workaround-for-emacs-bug nil)
(setq magit-repository-directories '(("~/git" . 1))
      magit-save-repository-buffers nil)
(setq transient-values '((magit-commit "--gpg-sign=152E3E3F7C4CCE44")
                         (magit-rebase "--gpg-sign=152E3E3F7C4CCE44")
                         (magit-pull   "--rebase" "--gpg-sign=152E3E3F7C4CCE44")))
(setq magit-repolist-columns
      '(("Repository" 25 magit-repolist-column-ident                  ())
        ("Version "   30 magit-repolist-column-version                ((:right-align t)))
        (" ⚡"          2 magit-repolist-column-dirty                  ((:right-align t)))
        (""           3 magit-repolist-column-branches               ((:right-align t)))
        ("≣"           3 magit-repolist-column-stashes                ((:right-align t)))
        ("⤓"           3 magit-repolist-column-unpulled-from-upstream ((:right-align t)))
        ("⤒"           3 magit-repolist-column-unpushed-to-upstream   ((:right-align t)))
        ("Path"       99 magit-repolist-column-path                   ())))

(eval-after-load 'vc-msg-git
  '(progn
     (setq vc-msg-git-show-commit-function 'magit-show-commit)
     (push '("m"
             "[m]agit-find-file"
             (lambda ()
               (let* ((info vc-msg-previous-commit-info)
                      (git-dir (locate-dominating-file default-directory ".git")))
                 (magit-find-file (plist-get info :id )
                                  (concat git-dir (plist-get info :filename))))))
           vc-msg-git-extra)))

;; -- deft -------------------------------------------------------------------
(setq deft-directory "~/org/z"
      deft-recursive nil
      deft-use-filename-as-title t)

;; -- company ----------------------------------------------------------------
(after! company
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 3
        company-show-numbers nil))

;; -- ess/julia---------------------------------------------------------------
(setq ess-use-eldoc t
      ess-eldoc-show-on-symbol t
      ess-execute-in-process-buffer t)
(add-hook 'inferior-ess-mode-hook 'my/comint-mode-hook)

(setq lsp-julia-default-environment "~/.julia/environments/v1.5")

;; -- python -----------------------------------------------------------------
(setq python-shell-interpreter "python3")
(setq python-shell-interpreter-args "-m IPython --simple-prompt -i")
(setq flycheck-python-pycompile-executable "python3"
      flycheck-python-pylint-executable "python3"
      flycheck-python-flake8-executable "python3")

;; -- lsp --------------------------------------------------------------------
(setq lsp-eldoc-enable-hover nil  ;; trigger manually using K
      lsp-ui-doc-border "white"
      lsp-enable-links t
      lsp-symbol-highlighting-skip-current t)

(after! lsp-ui
  (add-hook 'lsp-ui-doc-frame-hook
            (lambda (frame _w)
              (set-face-attribute 'default frame :font "Iosevka" :height 125)))
  (setq lsp-diagnostic-package :auto
        lsp-ui-doc-enable nil
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-position 'top
        lsp-ui-doc-include-signature t
        lsp-ui-doc-max-width 60
        lsp-ui-imenu-colors '("#798cad" "#88b582")
        lsp-ui-peek-list-width 60
        lsp-ui-peek-peek-height 20))

(setq ccls-executable "~/local/ccls/Release/ccls")
(setq lsp-clients-clangd-executable "/usr/local/opt/llvm/bin/clangd")
(setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))
(after! lsp-clients
  (set-lsp-priority! 'clangd 1))

;; -- lisp/haskell -----------------------------------------------------------
(setq inferior-lisp-program "sbcl")

(setq geiser-active-implementations '(chez chicken mit racket))

(after! racket-mode
  (add-hook! racket-mode
             #'racket-smart-open-bracket-mode))

(flycheck-define-checker racket-review
  "check racket source code using racket-review"
  :command ("raco" "review" source)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":error:" (message) line-end)
   (warning line-start (file-name) ":" line ":" column ":warning:" (message) line-end))
  :modes racket-mode)
(add-to-list 'flycheck-checkers 'racket-review)

(after! cider
  (setq cider-eldoc-display-context-dependent-info t))

(after! haskell
  (add-hook 'haskell-mode-hook #'hindent-mode))

;; -- rust -------------------------------------------------------------------
(setq lsp-rust-server 'rust-analyzer)
(setq rustic-lsp-server 'rust-analyzer)

(setq lsp-rust-analyzer-display-chaining-hints t
      lsp-rust-analyzer-display-parameter-hints nil
      lsp-rust-analyzer-server-display-inlay-hints t
      lsp-rust-full-docs t)

;; -- org --------------------------------------------------------------------
(setq org-directory "~/org"
      org-agenda-files '("~/org/refile.org" "~/org/j/")
      ;; org-agenda-text-search-extra-files '("~/org/drafts/")
      org-babel-clojure-backend 'cider
      +org-capture-todo-file "~/org/todo.org"
      org-journal-dir "~/org/j/"
      inferior-R-program-name "/usr/local/bin/R"
      inferior-R-args "-q --no-save --no-restore"
      inferior-STA-program-name "/usr/local/bin/stata-mp"
      inferior-STA-start-args "-q")

(setq diary-file "~/.diary")
(setq calendar-week-start-day 1)

(use-package! org-fancy-priorities
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("■" "■" "■")))

;; (remove-hook 'org-mode-hook #'org-superstar-mode)

(after! org
  (pushnew! org-link-abbrev-alist '("papers" . "/Users/chl/Documents/Papers/"))
  (setq org-agenda-include-diary t
        org-journal-follow-mode t
        org-journal-enable-agenda-integration t
        org-journal-enable-cache t)
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i\n" :prepend t :kill-buffer t)
          ("m" "Mail" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO [#A] [[%:path][%:subject]] :mu4e:\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n"
           :immediate-finish t :prepend t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-notes-file "Inbox")
           "* %u %?\n%i\n%a" :prepend t :kill-buffer t)
          ("w" "Web link" entry (file+headline "urls.org" "Inbox")
           "* %? \n%U\n%(retrieve-url)\n" :prepend t :kill-buffer t)
          ("j" "Journal" entry
           (file+olp+datetree +org-capture-journal-file)
           "* %U %?\n%i\n%a" :prepend t)
          ("z" "Org/z notes" entry (file my/write-file)
           "* %?\n\n#+FILETAGS:\n\n" :prepend nil :kill-buffer t)
          ("p" "Templates for projects")
          ("pt" "Project todo" entry  ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("pn" "Project notes" entry  ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("pc" "Project changelog" entry  ; {project-root}/changelog.org
           (file+headline +org-capture-project-notes-file "Unreleased")
           "* TODO %?\n%i\n%a" :prepend t :kill-buffer t)
          ("o" "Centralized templates for projects")
          ("ot" "Project todo" entry
           (function +org-capture-central-project-todo-file)
           "* TODO %?\n %i\n %a"
           :heading "Tasks"
           :prepend nil)
          ("on" "Project notes" entry
           (function +org-capture-central-project-notes-file)
           "* %U %?\n %i\n %a"
           :heading "Notes"
           :prepend t)
          ("oc" "Project changelog" entry
           (function +org-capture-central-project-changelog-file)
           "* %U %?\n %i\n %a"
           :heading "Changelog"
           :prepend t)))

  (setq org-hide-emphasis-markers t
        org-tags-column 79
        org-startup-indented nil
        org-startup-folded 'fold
        org-catch-invisible-edits 'error
        org-startup-with-inline-images nil
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-highlight-latex-and-related '(latex)
        org-support-shift-select t
        org-ellipsis " ▼ "
        org-todo-keywords '((sequence "TODO" "STAR" "|" "DONE" "CANC"))
        org-log-done 'time
        org-id-locations-file "~/org/.orgids"
        org-default-notes-file "~/org/notes.org"
        org-default-todo-file "~/org/todos.org"
        org-bibtex-file "~/org/references.bib"
        org-export-with-author nil
        org-export-with-creator nil
        org-export-with-toc nil
        org-export-with-section-numbers nil
        org-html-postamble nil
        org-html-htmlize-output-type nil
        org-latex-pdf-process '("latexmk -pdf -f -outdir=%o %f")
        org-pandoc-options-for-html5 '((section-divs . t)
                                       (bibliography . "/Users/chl/org/references.bib")
                                       ;; https://is.gd/lt21EQ
                                       (template . "/Users/chl/.pandoc/templates/GitHub.html5"))
        org-pandoc-options-for-latex-pdf '((pdf-engine . "lualatex")
                                           (listings . t)
                                           ;; (biblatex . t)
                                           (bibliography . "/Users/chl/org/references.bib")
                                           (template . "/Users/chl/.pandoc/templates/eisvogel.latex"))))

;; -- mu/irc -----------------------------------------------------------------
(load! "lisp/mu4e")
(add-to-list 'org-modules 'org-mu4e)

(load! "lisp/irc")

(custom-set-variables
 '(safe-local-variable-values '((ispell-dictionary))))
