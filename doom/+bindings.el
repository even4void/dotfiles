;;; ~/.config/doom/+bindings.el -*- lexical-binding: t; -*-

;; Some additional keybindings for my Doom config.
;; Other bindings are defined in ~/.emacs.d/modules/config/default;
;; See +evil-bindings.el for usual Vim-like bindings

(map!
 :nv "gh"         #'lsp-ui-doc-glance
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
 :desc "Next workspace"           "&"  #'+workspace:switch-next
 :desc "Split horizontally"       "-"  #'split-window-below
 :desc "Split vertically"         "|"  #'split-window-right
 :desc "Remove split"             "="  #'delete-other-windows
 :desc "Swiper all"               "@"  #'doom/swiper-all-region-or-symbol
 :desc "Swap window"              ")"  #'ace-swap-window
 :desc "Switch layout"            "("  #'doom/window-layout-toggle
 :desc "Interactive regex"        "%"  #'isearch-forward-regexp
 :desc "Swiper"                   "/"  #'swiper
 :desc "Open vterm"               "!"  #'+vterm/here
 :desc "Dired"                    "d"  #'dired-jump
 :desc "Query replace"            "r"  #'query-replace-regexp
 :desc "Delete workspace"         "W"  #'+workspace/delete
 :desc "Org Capture"              "x"  #'org-capture
 :desc "Pop up scratch buffer"    "X"  #'doom/open-scratch-buffer
 :desc "Project drawer"           "z"  #'+treemacs/toggle
 :desc "Project error list"       "Z"  #'lsp-treemacs-errors-list

 (:prefix "s"
  :desc "Counsel ag"              "c" #'doom/counsel-region-or-symbol
  :desc "Counsel Dash"            "C" #'counsel-dash-at-point
  :desc "Replace (regex)"         "R" #'replace-regexp
  :desc "Dictionary (input)"      "W" #'osx-dictionary-search-input)
 (:prefix "c"
  :desc "Help (LSP)"              "h" #'lsp-describe-thing-at-point
  :desc "Help (Dash)"             "H" #'counsel-dash
  :desc "Changelog"               "l" #'change-log-find-file
  :desc "Code outline"            "o" #'lsp-ui-imenu
  :desc "Prettify"                "P" #'prettify-symbols-mode
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
  :desc "Popup hunk"              "-" #'git-gutter:popup-hunk)
 (:prefix "n"
  :desc "Add bookmark"            "B" #'bookmark-set
  :desc "Gist region/buffer"      "g" #'gist-region-or-buffer
  :desc "Interleave"              "I" #'interleave-mode
  :desc "Markdown hide/show"      "M" #'markdown-toggle-markup-hiding
  :desc "Org narrow"              "N" #'org-narrow-to-subtree
  :desc "Org clean results"       "r" #'org-remove-all-result-blocks
  :desc "Org sort entries"        "R" #'org-sort-entries
  :desc "Insert URL"              "u" #'insert-url
  :desc "Select dictionary"       "Z" #'ispell-change-dictionary)
 (:prefix "o"
  :desc "Github"                  "g" #'open-github
  :desc "Geiser REPL"             "G" #'geiser
  :desc "IELM"                    "i" #'ielm
  :desc "IRC"                     "I" #'=irc)
 (:prefix "t"
  :desc "Tab line mode"           "a" #'global-tab-line-mode
  :desc "Beautify buffer"         "B" #'format-all-mode
  :desc "Flycheck"                "c" #'flycheck-buffer
  :desc "Rainbow colors"          "C" #'rainbow-mode
  :desc "Auto fill"               "f" #'auto-fill-mode
  :desc "Select theme"            "t" #'counsel-load-theme
  :desc "Toggle letter case"      "L" #'toggle-letter-case
  :desc "Show/Hide modeline"      "M" #'global-hide-mode-line-mode
  :desc "Poly Markdown"           "P" #'poly-markdown-mode
  :desc "Ruler"                   "R" #'display-fill-column-indicator-mode
  :desc "Cycle theme"             "T" #'cycle-theme
  :desc "Undo tree"               "u" #'undo-tree-visualize
  :desc "Unfill region"           "U" #'unfill-region
  :desc "Spellcheck"              "W" #'flyspell-buffer
  :desc "Switch dictionary"       "z" #'ispell-cycle-dictionary
  :desc "Select dictionary"       "Z" #'ispell-change-dictionary)
 (:prefix "w"
  :desc "Swap window H/V"         "w" #'doom/window-layout-toggle))
