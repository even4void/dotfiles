;;; ~/.config/doom/lisp/modeline.el -*- lexical-binding: t; -*-

(setq doom-modeline-bar-width 1
      doom-modeline-buffer-file-name-style 'relative-to-project
      doom-modeline-enable-word-count t
      doom-modeline-icon nil
      doom-modeline-buffer-encoding nil
      doom-modeline-major-mode-icon nil
      doom-modeline-percent-position nil
      doom-modeline-vcs-max-length 28)

;; Code adapted from https://github.com/angrybacon/dotemacs
(after! doom-modeline
  (doom-modeline-def-segment my/buffer
    "The buffer description and major mode icon."
    (let* ((active (doom-modeline--active))
           (face (if active 'match 'mode-line-inactive)))
      (propertize (concat " " (doom-modeline--buffer-name) " ") 'face face)
            ))

  (doom-modeline-def-segment my/buffer-position
    "The buffer position."
    (let* ((active (doom-modeline--active))
           (face (if active 'mode-line 'mode-line-inactive)))
      (propertize (concat (doom-modeline-spc)
                          (format-mode-line "%l:%c")
                          (doom-modeline-spc))
                  'face face)))

  (doom-modeline-def-segment my/buffer-simple
    "The buffer name but simpler."
    (let* ((active (doom-modeline--active))
           (face (cond ((and buffer-file-name (buffer-modified-p)) 'doom-modeline-buffer-modified)
                       (active 'match)
                       (t 'mode-line-inactive))))
      (concat (doom-modeline-spc)
              (propertize "%b" 'face face)
              (doom-modeline-spc))))

  (doom-modeline-def-segment my/default-directory
    "The buffer directory."
    (let* ((active (doom-modeline--active))
           (face (if active 'match 'mode-line-inactive)))
      (concat (doom-modeline-spc)
              (propertize (abbreviate-file-name default-directory) 'face face)
              (doom-modeline-spc))))

  (doom-modeline-def-segment my/flycheck
    "The error status with color codes and icons."
    (when (bound-and-true-p flycheck-mode)
      (let ((active (doom-modeline--active))
            (icon doom-modeline--flycheck-icon)
            (text doom-modeline--flycheck-text))
        (concat
         ;; (when icon
         ;;   (if active (propertize (concat " " icon " ") 'face 'mode-line) (doom-modeline-propertize-icon icon 'mode-line-inactive)))
         (when text
           (if active (propertize (concat " " text " ") 'face 'flycheck-info) (propertize text 'face 'mode-line-inactive)))
         (when (or icon text)
           (doom-modeline-spc))))))

  (doom-modeline-def-segment my/info
    "The topic and nodes in Info buffers."
    (let ((active (doom-modeline--active)))
      (concat
       (propertize " (" 'face (if active 'mode-line 'mode-line-inactive))
       (propertize (if (stringp Info-current-file)
                       (replace-regexp-in-string
                        "%" "%%"
                        (file-name-sans-extension (file-name-nondirectory Info-current-file)))
                     (format "*%S*" Info-current-file))
                   'face (if active 'doom-modeline-info 'mode-line-inactive))
       (propertize ") " 'face (if active 'mode-line 'mode-line-inactive))
       (when Info-current-node
         (propertize (concat (replace-regexp-in-string "%" "%%" Info-current-node)
                             (doom-modeline-spc))
                     'face (if active 'doom-modeline-buffer-path 'mode-line-inactive))))))

  (doom-modeline-def-segment my/major-mode
    "The current major mode, including environment information."
    (let* ((active (doom-modeline--active))
           (face (if active 'mode-line 'mode-line-inactive)))
      (concat (propertize (format-mode-line
                           (concat " " (when (and doom-modeline-env-version doom-modeline-env--version)
                                         (format "[%s] " doom-modeline-env--version)) mode-name " ")) 'face face))))
  (doom-modeline-def-segment my/process
    "The ongoing process details."
    (let ((result (format-mode-line mode-line-process)))
      (concat (if (doom-modeline--active)
                  (concat " " result)
                (propertize (concat " " result) 'face 'mode-line-inactive))
              (doom-modeline-spc))))

  (doom-modeline-def-segment my/modals
    "Display modal editing states."
    (when (doom-modeline--active)
      (doom-modeline-segment--modals)))

  (doom-modeline-def-segment my/space
    "A simple space."
    (doom-modeline-spc))

  (doom-modeline-def-segment my/vcs
    "The version control system information."
    (when-let ((branch doom-modeline--vcs-text))
      (let ((active (doom-modeline--active))
            (text (concat "  " branch " ")))
        (concat (if active
                    (propertize text 'face 'match)
                  (propertize text 'face 'mode-line-inactive))))))

  (doom-modeline-mode 1)

  (doom-modeline-def-modeline 'info
    '(bar my/modals my/buffer my/info selection-info)
    '(irc-buffers matches my/process debug my/buffer-position my/major-mode))
  (doom-modeline-def-modeline 'main
    '(bar my/modals my/buffer my/flycheck remote-host selection-info)
    '(irc-buffers matches my/vcs my/process debug my/buffer-position my/major-mode))
  (doom-modeline-def-modeline 'message
    '(bar my/modals my/buffer-simple selection-info)
    '(irc-buffers matches my/process my/buffer-position my/major-mode))
  (doom-modeline-def-modeline 'org-src
    '(bar my/modals my/buffer-simple my/flycheck selection-info)
    '(irc-buffers matches my/process debug my/buffer-position my/major-mode))
  (doom-modeline-def-modeline 'package
    '(bar my/modals my/space package)
    '(irc-buffers matches my/process debug my/major-mode))
  (doom-modeline-def-modeline 'project
    '(bar my/modals my/default-directory)
    '(irc-buffers matches my/process debug my/major-mode))
  (doom-modeline-def-modeline 'special
    '(bar my/modals my/buffer selection-info)
    '(irc-buffers matches my/process debug my/buffer-position my/major-mode))
  (doom-modeline-def-modeline 'vcs
    '(bar my/modals my/buffer remote-host selection-info)
    '(irc-buffers matches my/process debug my/buffer-position my/major-mode)))
