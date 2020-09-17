;;; ~/.config/doom/+bindings.el -*- lexical-binding: t; -*-

;; Some additional keybindings for my Doom config.
;; Other bindings are defined in ~/.emacs.d/modules/config/default;
;; See +evil-bindings.el for usual Vim-like bindings

(map!
 :nv "gh"         #'lsp-ui-doc-glance
 :n "-"           #'dired-jump
 :n "§"           #'evil-avy-goto-char-timer
 :i "C-n"         #'dabbrev-expand
 :v "J"           (concat ":m '>+1" (kbd "RET") "gv=gv")
 :v "K"           (concat ":m '<-2" (kbd "RET") "gv=gv")

 ;; OSX shortcuts without cua-mode (GUI only)
 :n "s-<return>"  #'toggle-frame-maximized
 "s-z"            #'undo
 "s-c"            (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
 "s-v"            #'yank
 "s-x"            #'kill-region
 "s-s"            #'save-buffer
 "s-a"            #'mark-whole-buffer
 "s-;"            #'eval-expression
 "s-r"            #'query-replace
 "s-p"            #'counsel-M-x
 "s-b"            #'persp-switch-to-buffer
 "s-@"            #'lsp-ui-imenu
 "s-&"            #'+workspace:switch-next
 "s-!"            #'swiper-isearch
 "s-)"            #'ace-swap-window
 "s-("            #'doom/window-layout-toggle

 ;; window/workspace (GUI only)
 :ni "s-<up>"     #'evil-window-up
 :ni "s-<down>"   #'evil-window-down
 :ni "s-<left>"   #'evil-window-left
 :ni "s-<right>"  #'evil-window-right

 (:map markdown-mode-map ;; (GUI only)
  :i "s-i" #'markdown-insert-italic
  :i "s-b" #'markdown-insert-bold)

 (:after helpful
  (:map helpful-mode-map
   :n "RET"    #'helpful-visit-reference
   :n "o"      #'ace-link-help
   :n "q"      #'quit-window
   :n "Q"      #'ivy-resume))

 (:after ess
  (:map ess-r-mode-map
   :ni "C-<return>"  #'ess-eval-line))

 (:after python
  (:map python-mode-map
   :n "C-<return>"  #'python-shell-send-region-or-line))

 (:after osx-dictionary
  :map osx-dictionary-mode-map
  :n "q" #'osx-dictionary-quit
  :n "r" #'osx-dictionary-read-word
  :n "s" #'osx-dictionary-search-input
  :n "o" #'osx-dictionary-open-dictionary.app)

 ;; which-key items
 :leader
 :desc "Next workspace"           "&" #'+workspace:switch-next
 :desc "Make"                     "§" #'+make/run
 :desc "Remove split"             "-" #'delete-other-windows
 :desc "Counsel Rg"               "@" #'counsel-rg
 :desc "Interactive regex"        "%" #'isearch-forward-regexp
 :desc "Swiper"                   "/" #'swiper
 :desc "Open vterm"               "$" #'+vterm/here
 :desc "Shell cmd"                "!" #'shell-command
 :desc "Prev buffer"              "j" #'evil-prev-buffer
 :desc "Next buffer"              "k" #'evil-next-buffer
 :desc "Next window"              ")" #'evil-window-next
 :desc "Query replace"            "r" #'query-replace-regexp
 :desc "Org Capture"              "x" #'org-capture
 :desc "Pop up scratch buffer"    "X" #'doom/open-scratch-buffer
 :desc "Spellcheck"               "W" #'flyspell-buffer
 :desc "Delete workspace"         "z" #'+workspace/delete

 (:prefix "s"
  :desc "Counsel rg (selection)"  "c" #'doom/counsel-region-or-symbol
  :desc "Counsel Dash"            "C" #'counsel-dash-at-point
  :desc "Counsel fzf"             "F" #'counsel-fzf
  :desc "Replace (regex)"         "R" #'replace-regexp
  :desc "Dictionary (input)"      "W" #'osx-dictionary-search-input
  :desc "Counsel fzf occur"       "z" #'counsel-fzf-occur)
 (:prefix "c"
  :desc "Help (LSP)"              "h" #'lsp-describe-thing-at-point
  :desc "Help (Dash)"             "H" #'counsel-dash
  :desc "Changelog"               "l" #'change-log-find-file
  :desc "Insert License"          "L" #'+file-templates/insert-license
  :desc "Code outline"            "o" #'lsp-ui-imenu
  :desc "Prettify"                "P" #'prettify-symbols-mode
  :desc "Sort lines"              "S" #'sort-lines
  :desc "Select checker"          "y" #'flycheck-select-checker
  :desc "Available checkers"      "z" #'flycheck-verify-setup)
 (:prefix-map ("e" . "export")
  :desc "Org HTML publish"        "=" #'org-publish-all
  :desc "Org dispatcher"          "e" #'org-export-dispatch
  :desc "Org HTML+Pandoc"         "h" #'org-pandoc-export-to-html5-and-open
  :desc "Org HTML"                "H" #'org-html-export-to-html
  :desc "Markdown open"           "m" #'markdown-open
  :desc "Org PDF+Pandoc"          "p" #'org-pandoc-export-to-latex-pdf-and-open
  :desc "Org PDF+Latex"           "P" #'org-latex-export-to-pdf)
 (:prefix "g"
  :desc "Popup hunk"              "-" #'git-gutter:popup-hunk
  :desc "VC msg"                  "M" #'vc-msg-show)
 (:prefix "n"
  :desc "Add bookmark"            "B" #'bookmark-set
  :desc "Gist region/buffer"      "g" #'gist-region-or-buffer
  :desc "Interleave"              "I" #'interleave-mode
  :desc "Org narrow"              "N" #'org-narrow-to-subtree
  :desc "Org clean results"       "r" #'org-remove-all-result-blocks
  :desc "Insert URL"              "u" #'insert-url
  :desc "Unfill region"           "U" #'unfill-region)
 (:prefix "o"
  :desc "Calendar"                "c" #'calendar
  :desc "Elfeed"                  "E" #'elfeed
  :desc "Github"                  "g" #'open-github
  :desc "Geiser REPL"             "G" #'geiser
  :desc "IELM"                    "i" #'ielm
  :desc "IRC"                     "I" #'=irc)
 (:prefix "t"
  :desc "Tab line mode"           "a" #'global-tab-line-mode
  :desc "Code format mode"        "B" #'format-all-mode
  :desc "Flycheck"                "c" #'flycheck-buffer
  :desc "Rainbow colors"          "C" #'rainbow-mode
  :desc "Org indent mode"         "d" #'org-indent-mode
  :desc "Auto fill"               "f" #'auto-fill-mode
  :desc "Select theme"            "t" #'counsel-load-theme
  :desc "Toggle letter case"      "L" #'toggle-letter-case
  :desc "Markdown hide/show"      "m" #'markdown-toggle-markup-hiding
  :desc "Show/Hide modeline"      "M" #'global-hide-mode-line-mode
  :desc "Ruler"                   "R" #'display-fill-column-indicator-mode
  :desc "Server"                  "S" #'server-start
  :desc "Undo tree"               "u" #'undo-tree-visualize
  :desc "Spellcheck"              "W" #'flyspell-buffer
  :desc "Switch dictionary"       "z" #'ispell-cycle-dictionary
  :desc "Select dictionary"       "Z" #'ispell-change-dictionary)
 (:prefix "w"
  :desc "Swap window H/V"         "w" #'doom/window-layout-toggle))
