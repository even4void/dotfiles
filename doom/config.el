;;; ~/.config/doom/config.el -*- lexical-binding: t; -*-

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 175))

(setq user-full-name "chl"
      user-mail-address "chl@aliquote.org")

(if (display-graphic-p)
    ;; Use a patched font for GUI mode so that we get Iosevka ligatures that we have
    ;; free when using iTerm.
    (setq doom-font (font-spec :family "Iosevka" :size 14)
          doom-variable-pitch-font (font-spec :family "Iosevka" :size 14)))

(load! "+bindings")

;; ---------------------------------------------------------------------------
;; ui
;; ---------------------------------------------------------------------------
;; doom-themes already comes with a custom Nord theme but I don't like it
;; (moreover we must activate a 24-bit mode for the terminal, see ~/.terminfo).
;; So here we go, with the true https://github.com/arcticicestudio/nord-emacs.
(setq nord-comment-brightness 15)
(setq nord-region-highlight "frost")
(setq nord-uniform-mode-lines t)
(load-theme 'nord t)
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
  (doom-modeline-def-modeline 'my/modeline
    '(bar matches buffer-info remote-host buffer-position parrot selection-info)
    '(misc-info minor-modes checker input-method buffer-encoding major-mode process vcs))
  (defun setup-custom-doom-modeline ()
    (doom-modeline-set-modeline 'my/modeline 'default))
  (add-hook 'doom-modeline-mode-hook 'setup-custom-doom-modeline))
(setq doom-neotree-file-icons nil)
(setq doom-modeline-env-python-executable "python3")
(setq doom-modeline-enable-word-count t)
(setq doom-modeline-indent-info t)
; (setq doom-modeline-icon t)
(setq doom-modeline-checker-simple-format t)

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


;; -- web & doc --------------------------------------------------------------
(setq browse-url-browser-function 'eww-browse-url)
;; NOTE We can still hit `&' to open the page in an external browser
;; this is mainly to read the Hyperspec doc inline. Note, however, that dash-docs
;; already provides the Hyperspec, so we don't really need our local version.

;; -- bibtex -----------------------------------------------------------------
(after! bibtex
  (setq bibtex-field-delimiters 'double-quotes
        bibtex-autokey-year-length 4
        bibtex-autokey-name-year-separator "-"
        bibtex-autokey-year-title-separator "-"
        bibtex-autokey-titleword-separator "-"
        bibtex-autokey-titlewords 2
        bibtex-autokey-titlewords-stretch 1
        bibtex-autokey-titleword-length 5))

(after! ivy-bibtex
  (setq bibtex-completion-bibliography '("~/Documents/Drafts/references.bib")
        bibtex-completion-library-path '("~/Documents/Papers"
                                         "~/Documents/Papers/_iBooks")
        bibtex-completion-pdf-extension '(".pdf" ".epub")
        bibtex-completion-notes-path "~/org/papers.org"
        bibtex-completion-notes-symbol "≣"
        bibtex-completion-pdf-symbol "◉"
        bibtex-completion-additional-search-fields '(keywords)
        bibtex-completion-pdf-open-function (lambda (fpath) (call-process
                                                        "open" nil 0 nil
                                                        "-a" "/Applications/Preview.app"
                                                        fpath))
        bibtex-completion-display-formats
        '((t . "${author:30} ${title:*} ${year:4} ${=type=:8} ${=has-pdf=:1} ${=has-note=:1}"))
        bibtex-completion-cite-prompt-for-optional-arguments nil
        bibtex-completion-format-citation-functions
        '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
          (latex-mode    . bibtex-completion-format-citation-cite)
          (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
          (default       . bibtex-completion-format-citation-default))
        bibtex-completion-notes-template-one-file
        (format "* [[/Users/chl/Documents/Papers/${=key=}.pdf][${=key=}]] - ${title}\n :PROPERTIES:\n :Custom_ID: ${=key=}\n :INTERLEAVE_PDF:/Users/chl/Documents/Papers/${=key=}.pdf\n :END:\n"))
  (advice-add 'bibtex-completion-candidates
              :filter-return 'reverse))

(setq org-ref-default-bibliography '("~/Documents/Drafts/references.bib")
      org-ref-pdf-directory "~/Documents/Papers"
      reftex-default-bibliography '("~/Documents/Drafts/references.bib"))
(after! org-ref
  (setq reftex-default-bibliography '("~/Documents/Drafts/references.bib")
        org-ref-bibliography-notes "~/org/papers.org"
        org-ref-default-bibliography '("~/Documents/Drafts/references.bib")
        org-ref-pdf-directory "~/Documents/Papers"
        org-ref-note-title-format "* [[/Users/chl/Documents/Papers/%k.pdf][%k]] - %t\n:PROPERTIES:\n :Custom_ID: %k\n :INTERLEAVE_PDF:/Users/chl/Documents/Papers/%k.pdf\n :END:\n"))

;; -- flycheck ---------------------------------------------------------------
(setq flycheck-python-pycompile-executable "python3"
      flycheck-python-pylint-executable "python3"
      flycheck-python-flake8-executable "python3")

;; -- text/markdown editing --------------------------------------------------
(setq time-stamp-active t
      time-stamp-line-limit 10)
(setq show-trailing-whitespace t)
(setq +format-on-save-enabled-modes
  '(not emacs-lisp-mode  ; elisp's mechanisms are good enough
        sql-mode         ; sqlformat is currently broken
        python-mode))    ; because I don't like it
(add-hook 'before-save-hook 'delete-trailing-whitespace)
; (setq require-final-newline t)
(remove-hook 'dired-mode-hook 'diredfl-mode)
(add-hook 'write-file-functions 'time-stamp)
(auto-fill-mode -1)
(remove-hook 'text-mode-hook #'turn-on-auto-fill)
(add-hook 'text-mode-hook #'turn-on-visual-line-mode)
;; too much noise (e.g., Magit commit messages)
;; (add-hook 'text-mode-hook 'flyspell-mode)
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
  (remove-hook 'markdown-mode-hook #'delete-trailing-whitespace)
  (remove-hook 'markdown-mode-hook #'turn-on-auto-fill)
  (add-hook 'markdown-mode-hook #'turn-on-visual-line-mode))

;; -- pretty-code ------------------------------------------------------------
;; Best with custom Iosevka font, see https://github.com/ar1a/dotfiles/tree/master/emacs/.doom.d
(setq +pretty-code-enabled-modes '(emacs-lisp-mode org-mode clojure-mode
                                   latex-mode scheme-mode racket-mode ess-r-mode))

;; -- dash-docs --------------------------------------------------------------
(setq dash-docs-enable-debugging nil)
;; FIXME We should enable docset only when in major mode
;; (setq dash-docs-common-docsets '("Emacs Lisp" "Common Lisp" "Clojure" "Racket"))

;; -- eshell -----------------------------------------------------------------
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

;; -- git/magit ---------------------------------------------------------------
;; See https://github.com/magit/ghub/issues/81
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3"
      ghub-use-workaround-for-emacs-bug nil)
(setq magit-repository-directories '(("~/git" . 1))
      magit-save-repository-buffers nil
      magit-revision-show-gravatars nil
      transient-values '((magit-commit "--gpg-sign=152E3E3F7C4CCE44")
                         (magit-rebase "--autosquash" "--gpg-sign=152E3E3F7C4CCE44")
                         (magit-pull "--rebase" "--gpg-sign=152E3E3F7C4CCE44")))

;; -- neotree ----------------------------------------------------------------
(setq neo-smart-open t
      neo-vc-integration '(face)
      projectile-switch-project-action 'neotree-projectile-action)

;; -- ox-hugo ----------------------------------------------------------------
(setq org-hugo-default-section-directory "micro"
      org-hugo-default-base-dir "~/Sites/aliquote")

;; -- deft -------------------------------------------------------------------
(setq deft-extensions '("org" "md" "txt")
      deft-directory "~/Documents/Drafts"
      deft-text-mode 'markdown-mode
      deft-use-filename-as-title t
      deft-recursive t
      deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))

;; -- org --------------------------------------------------------------------
(setq org-directory "~/org"
      org-babel-clojure-backend 'cider
      inferior-R-program-name "/usr/local/bin/R"
      inferior-STA-program-name "/usr/local/bin/stata-mp")
(after! org
  (setq org-capture-templates
      ;; (retrieve-url) is defined in autoload/
      '(("b" "Blog" entry (file+headline "micro.org" "Micro")
         "** TODO %?\n:PROPERTIES:\n:EXPORT_FILE_NAME:\n:END:\n%^g\n" :empty-lines 1)
        ("d" "Diary" entry (file+olp+datetree "diary.org" "Diary")
         "* %?\n%T\n  %i\nFrom: %a")
        ("m" "Meetings" entry (file+headline "diary.org" "Meetings")
         "** MEET with %? :MEETING:\n%t" :clock-in t :clock-resume t)
        ("n" "Notes" entry (file+headline org-default-notes-file "Notes")
         "* %u %?\n%i" :prepend t :kill-buffer t)
        ("p" "Projects todo" entry (file+headline org-default-todo-file "Projects")
         "* TODO %? %^g \n %i\n")
        ("t" "Todo" entry (file+headline org-default-todo-file "Tasks")
         "* TODO %? %^g \n %i\n")
        ("w" "Web link" entry (file+headline org-default-notes-file "Inbox")
         "* %? \n%U\n%(retrieve-url)\n")))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (python . t)
     (C . t)
     (R . t)
     (stata . t)
     (lisp . t)
     (emacs-lisp . t)))
  (setq org-hide-emphasis-markers t
        org-tags-column 79
        org-highlight-links '(bracket angle plain radio tag date footnote cite)
        org-startup-with-inline-images nil
        org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-support-shift-select t
        org-src-tab-acts-natively nil
        ;; org-bullets-bullet-list '("#")
        org-ellipsis " ▼ "
        org-todo-keywords '((sequence "TODO" "STAR" "|" "DONE" "CANC"))
        org-log-done 'time
        org-default-notes-file "~/org/notes.org"
        org-default-todo-file "~/org/todos.org"
        org-bibtex-file "~/Documents/Drafts/references.bib"
        org-latex-pdf-process '("latexmk -pdf -f -outdir=%o %f")
        org-pandoc-options '((standalone . t)
                             (bibliography . "~/Documents/Drafts/references.bib")))
  (add-hook 'org-mode-hook #'turn-on-visual-line-mode))

;; --tex ---------------------------------------------------------------------
(setq TeX-auto-save nil
      TeX-view-program-selection '((output-pdf "PDF Tools"))
      TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
      TeX-source-correlate-method 'synctex
      TeX-source-correlate-start-server t)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)

;; -- mu4e -------------------------------------------------------------------
;; NOTE Gmail is in read-only mode (2019-05); deactivated again (2019-08)
(after! mu4e
  (setq mu4e-get-mail-command "mbsync -a"
        ; smtpmail-stream-type 'starttls
        mu4e-change-filenames-when-moving t
        mu4e-maildirs-extension-default-collapse-level 0
        mu4e-maildirs-extension-maildir-expanded-prefix "»"
        mu4e-maildirs-extension-maildir-default-prefix "◉"
        mu4e-maildirs-extension-toggle-maildir-key "+"
        mu4e-compose-format-flowed t
        mu4e-headers-show-threads nil
        mu4e-headers-date-format "%Y-%m-%d %H:%M"
        mu4e-confirm-quit nil
        ; mu4e-completing-read-function 'completing-read
        smtpmail-queue-dir "~/.mail/queue/cur"
        smtpmail-auth-credentials (expand-file-name "~/.authinfo.gpg")
        mu4e-maildir "~/.mail"
        mu4e-attachment-dir "~/Downloads")
  (setq mu4e-headers-fields
        '( (:date          .  25)
           (:flags         .   6)
           (:mailing-list  .  10)
           (:from          .  22)
           (:subject)))
  (remove-hook 'mu4e-compose-mode-hook #'flyspell-mode)
  (remove-hook 'mu4e-compose-mode-hook #'org-mu4e-compose-org-mode)
  (add-hook 'mu4e-compose-mode-hook
          (lambda () (local-set-key (kbd "C-c C-w") #'mu4e-choose-signature)))
  (setq mu4e-user-mail-address-list '("ch.lalanne@aliquote.org" "ch.lalanne@mac.com"))
  (setq mu4e-contexts
        `( ,(make-mu4e-context
             :name "i icloud"
             :enter-func (lambda () (mu4e-message "Enter ch.lalanne@mac.com context"))
             :leave-func (lambda () (mu4e-message "Leave ch.lalanne@mac.com context"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg :to "ch.lalanne@mac.com")))
             :vars '((user-mail-address      . "ch.lalanne@mac.com")
                     (user-full-name         . "Christophe Lalanne")
                     (mu4e-sent-folder       . "/icloud/Sent Messages")
                     (mu4e-drafts-folder     . "/icloud/Drafts")
                     ;; ( mu4e-trash-folder      . "/icloud/Trash" )
                     (smtpmail-smtp-server   . "smtp.mail.me.com")
                     (smtpmail-stream-type   . starttls)
                     (smtpmail-smtp-service  . 587)
                     (mu4e-compose-signature . (concat "chl\n"))))

           ,(make-mu4e-context
             :name "a aliquote"
             :enter-func (lambda () (mu4e-message "Enter chl@aliquote.org context"))
             :match-func (lambda (msg)
                           (when msg
                             (mu4e-message-contact-field-matches msg :to "chl@aliquote.org")))
             :vars '((user-mail-address       . "ch.lalanne@aliquote.org")
                     (user-full-name          . "Christophe Lalanne")
                     (mu4e-sent-folder        . "/aliquote/Sent")
                     (mu4e-drafts-folder      . "/aliquote/Drafts")
                     (mu4e-trash-folder       . "/aliquote/Trash")
                     (smtpmail-smtp-server    . "ssl0.ovh.net")
                     (smtpmail-smtp-service   . 587)
                     (mu4e-compose-signature  . (concat "chl\n"))))))
  (setq mu4e-context-policy 'pick-first
        mu4e-compose-context-policy nil)
  (add-to-list 'mu4e-bookmarks
               '("maildir:/aliquote/INBOX OR maildir:/icloud/INBOX" "All Inboxes" ?i)))

;; -- company ----------------------------------------------------------------
(after! company
  (setq company-idle-delay 0.1
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

;; -- python -----------------------------------------------------------------
(setq python-shell-interpreter "python3")
(setq python-shell-interpreter-args "-m IPython --simple-prompt -i")
(setq flycheck-python-pycompile-executable "python3"
      flycheck-python-pylint-executable "python3"
      flycheck-python-flake8-executable "python3")
(after! lsp
  ;; For whatever reason, I have to activate flake8 manually; otherwise we end up with
  ;; pyflakes! And the following doesn't seem to work either.
  ;; (setq lsp-clients-python-settings '(:configurationSources ["flake8"]))
  (setq-default lsp-pyls-configuration-sources ["flake8"])
  (setq lsp-ui-flycheck-enable t
        lsp-pyls-plugins-pylint-enabled nil
        lsp-pyls-plugins-pyflakes-enabled nil))

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
