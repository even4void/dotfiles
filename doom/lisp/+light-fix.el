;;; ~/.config/doom/+light-fix.el -*- lexical-binding: t; -*-

;; Just overriding the default icons for Flycheck.
(add-hook! '(flycheck-status-changed-functions
             flycheck-mode-hook)
  (defun +modeline-checker-update (&optional status)
    "Update flycheck text via STATUS."
    (setq +modeline-checker
          (pcase status
            (`finished
             (if flycheck-current-errors
                 (let-alist (flycheck-count-errors flycheck-current-errors)
                   (let ((error (or .error 0))
                         (warning (or .warning 0))
                         (info (or .info 0)))
                     (+modeline-format-icon "error_outline"
                                            (number-to-string (+ error warning info))
                                            (cond ((> error 0)   'error)
                                                  ((> warning 0) 'warning)
                                                  ('success))
                                            (format "Errors: %d, Warnings: %d, Debug: %d"
                                                    error
                                                    warning
                                                    info))))
               (+modeline-format-icon "check" "#" 'font-lock-variable-name-face)))
            (`running     (+modeline-format-icon "code" "*" 'font-lock-doc-face "Running..."))
            (`errored     (+modeline-format-icon "info_outline" "#" 'error "Errored!"))
            (`interrupted (+modeline-format-icon "pause_circle_outline" "!" 'font-lock-comment-face "Interrupted"))
            (`suspicious  (+modeline-format-icon "no_sim" "!" 'warning "Suspicious"))))))
