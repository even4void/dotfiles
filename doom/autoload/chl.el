;;; ~/.config/doom/autoload/chl.el -*- lexical-binding: t; -*-

;;;###autoload
(defun my/comint-mode-hook ()
    (local-set-key '[up] 'comint-previous-input)
    (local-set-key '[down] 'comint-next-input))

;;;###autoload
(defun retrieve-url ()
    "Retrieve the URL of the current Safari page as a string."
    (org-trim (shell-command-to-string
               "osascript -e 'tell application \"Safari\" to return URL of document 1'")))

;;;###autoload
(defun insert-url ()
  "Insert URL of current browser page into Emacs buffer."
  (interactive)
  (insert (retrieve-url)))

;;;###autoload
(defun unfill-paragraph ()
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

;;;###autoload
(defun unfill-region ()
  "Unfill a region, i.e., make text in that region not wrap."
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))

;;;###autoload
(defun org-remove-all-result-blocks ()
  "Remobe Org Babel results."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "#+begin_src " nil t)
      (org-babel-remove-result))))

;;;###autoload
(defun mu4e-choose-signature ()
  "Insert one of a number of sigs"
  (interactive)
  (let ((message-signature
          (mu4e-read-option "Signature:"
            '(("formal" .
              (concat
           "Christophe Lalanne\n"
           "University Paris-Diderot, LIED/GEC\n"
           "www.aliquote.org\n"))
               ("informal" .
              "chl\n")))))
    (message-insert-signature)))

;;;###autoload
(defun create-scratch-buffer-current-mode ()
  "Create a new scratch buffer to work in and set its mode to current `major-mode'."
  (interactive)
  (create-scratch-buffer major-mode))

;;;###autoload
(defun toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Toggles from 3 cases: UPPER CASE, lower case, Title Case,
in that cyclic order."
  (interactive)
  (let (pos1 pos2 (deactivate-mark nil) (case-fold-search nil))
    (if (and transient-mark-mode mark-active)
        (setq pos1 (region-beginning)
              pos2 (region-end))
      (setq pos1 (car (bounds-of-thing-at-point 'word))
            pos2 (cdr (bounds-of-thing-at-point 'word))))

    (when (not (eq last-command this-command))
      (save-excursion
        (goto-char pos1)
        (cond
         ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state
                                                     "all lower"))
         ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state
                                                     "all caps") )
         ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state
                                                     "init caps") )
         (t (put this-command 'state "all lower") ))))

    (cond
     ((string= "all lower" (get this-command 'state))
      (upcase-initials-region pos1 pos2) (put this-command 'state "init caps"))
     ((string= "init caps" (get this-command 'state))
      (upcase-region pos1 pos2) (put this-command 'state "all caps"))
     ((string= "all caps" (get this-command 'state))
      (downcase-region pos1 pos2) (put this-command 'state "all lower")))))

;;;###autoload
(defun open-in-iterm ()
  "Go to present working dir and focus iterm"
  (interactive)
  (do-applescript
   (concat
    " tell application \"iTerm\"\n"
    "   tell the current session of current window\n"
    (format "     write text \"cd %s\" \n"
            ;; string escaping madness for applescript
            (replace-regexp-in-string "\\\\" "\\\\\\\\"
                                      (shell-quote-argument (or default-directory "~"))))
    "   end tell\n"
    " end tell\n"
    " do shell script \"open -a iTerm\"\n"
    ))
)

;;;###autoload
(defun doom/window-layout-toggle ()
  "Toggle between horizontal and vertical layout of two windows."
  (interactive)
  (if (= (count-windows) 2)
    (let* ((window-tree (car (window-tree)))
           (current-split-vertical-p (car window-tree))
           (first-window (nth 2 window-tree))
           (second-window (nth 3 window-tree))
           (second-window-state (window-state-get second-window))
           (splitter (if current-split-vertical-p
                         #'split-window-horizontally
                       #'split-window-vertically)))
      (delete-other-windows first-window)
      ;; `window-state-put' also re-selects the window if needed, so we don't
      ;; need to call `select-window'
      (window-state-put second-window-state (funcall splitter)))
    (error "Can't toggle window layout when the number of windows isn't two.")))

;;;###autoload
(defun doom/swiper-region-or-symbol ()
  "Run `swiper' with the selected region or the symbol
around point as the initial input."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (swiper input)))

;;;###autoload
(defun doom/swiper-all-region-or-symbol ()
  "Run `swiper-all' with the selected region or the symbol
around point as the initial input."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (swiper-all input)))

;;;###autoload
(defun doom/counsel-region-or-symbol ()
  "Run `counsel-ag' with the selected region or the symbol
around point as the initial input."
  (interactive)
  (let ((input (if (region-active-p)
                   (buffer-substring-no-properties
                    (region-beginning) (region-end))
                 (thing-at-point 'symbol t))))
    (counsel-ag input)))

;;;###autoload
(defun python-shell-send-region-or-line nil
  "Sends from python-mode buffer to a python shell, intelligently."
  (interactive)
  (cond ((region-active-p)
         (setq deactivate-mark t)
         (python-shell-send-region (region-beginning) (region-end)))
        (t (python-shell-send-current-statement))))

;;;###autoload
(defun python-shell-send-current-statement ()
  "Send current statement to Python shell. Taken from elpy-shell-send-current-statement"
  (interactive)
  (let ((beg (python-nav-beginning-of-statement))
        (end (python-nav-end-of-statement)))
    (python-shell-send-string (buffer-substring beg end)))
  (python-nav-forward-statement))

;;;###autoload
(defun my/write-file ()
  "Create a text file in a given directory"
  (interactive)
  (let ((name (read-string "Name: ")))
    (expand-file-name (format "%s-%s.org"
                              (format-time-string "%Y-%m-%d-%H-%M-%S") name) "~/org/z/")))

;;;###autoload
(defun ispell-cycle-dictionary ()
  "Cycle between fr <-> en dictionaries"
  (interactive)
  (let* ((curr ispell-current-dictionary)
         (next (if (string= curr "fr") "en" "fr")))
        (ispell-change-dictionary next)
        (message "Dictionary switched from %s to %s" curr next)))

(setq color-themes (list
                    'doom-opera-light
                    'doom-nord))

;;;###autoload
(defun open-github ()
  (interactive)
  (browse-url "https://github.com/even4void"))

;;;###autoload
(defun my/yank-org-link (text)
  (if (derived-mode-p 'org-mode)
      (insert text)
    (string-match org-bracket-link-regexp text)
    (insert (substring text (match-beginning 1) (match-end 1)))))

;;;###autoload
(defun my/org-copy-smart-url ()
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                 (or (caddr link-info) (point-max))))))
    (if (not text)
        (error "Not in org link")
      (add-text-properties 0 (length text) '(yank-handler (my-yank-org-link)) text)
      (kill-new text))))

;;;###autoload
(defun my/org-export-url ()
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                 (or (caddr link-info) (point-max))))))
    (if (not text)
        (error "Not in org link")
      (string-match org-bracket-link-regexp text)
      (kill-new (substring text (match-beginning 1) (match-end 1))))))

;;;###autoload
(defun my/org-open-safari ()
  (interactive)
  (let* ((link-info (assoc :link (org-context)))
         (text (when link-info
                 (buffer-substring-no-properties (or (cadr link-info) (point-min))
                                                 (or (caddr link-info) (point-max))))))
    (if (not text)
        (error "Not in org link")
      (string-match org-bracket-link-regexp text)
      (kill-new (substring text (match-beginning 1) (match-end 1)))
      (browse-url-generic text))))
