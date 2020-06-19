;;; ~/.config/doom/+bindings.el -*- lexical-binding: t; -*-

;; Some additional keybindings for my Doom config.
;; Other bindings are defined in ~/.emacs.d/modules/config/default;
;; See +evil-bindings.el for usual Vim-like bindings

(map!

 ;; dictionnary
 :n "C-c d"       #'osx-dictionary-search-word-at-point

 :n "s-<return>"  #'toggle-frame-fullscreen

 ;; OSX shortcuts without cua-mode (mostly redefined here)
 "s-z"        #'undo
 "s-c"        (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
 "s-v"        #'yank
 "s-x"        #'kill-region
 "s-s"        #'save-buffer
 "s-a"        #'mark-whole-buffer
 "s-b"        #'persp-switch-to-buffer
 "s-;"        #'eval-expression
 "s-r"        #'query-replace
 "s-p"        #'counsel-M-x
 "s-@"        #'+treemacs/toggle

 "C-f"        #'forward-word
 "C-b"        #'backward-word
 "C-a"        #'backward-sentence
 "C-e"        #'forward-sentence

 ;; window/workspace
 :ni "s-<left>"     #'evil-window-left
 :ni "s-<right>"    #'evil-window-right
 :ni "s-<up>"       #'evil-window-up
 :ni "s-<down>"     #'evil-window-down
 :ni "C-s-<left>"   #'+workspace/switch-left
 :ni "C-s-<right>"  #'+workspace/switch-right

 (:map markdown-mode-map
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

;; TODO Update jupyter-(repl-)mode-map

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
 :desc "Next workspace"        ">"  #'+workspace:switch-next
 :desc "Split horizontally"    "-"  #'split-window-below
 :desc "Split vertically"      "|"  #'split-window-right
 :desc "Remove split"          "="  #'delete-other-windows
 :desc "Swiper all"            "@"  #'doom/swiper-all-region-or-symbol
 :desc "Pop up scratch buffer" "X"    #'doom/open-scratch-buffer
 :desc "Org Capture"           "x"    #'org-capture
 (:prefix "s"
   :desc "Counsel ag"          "c" #'doom/counsel-region-or-symbol
   :desc "Counsel Dash"        "C" #'counsel-dash-at-point
   :desc "Dictionary (input)"  "W" #'osx-dictionary-search-input
   :desc "Replace (regex)"     "R" #'replace-regexp)
 (:prefix "c"
   :desc "Available checkers"  "z" #'flycheck-verify-setup
   :desc "Select checker"      "y" #'flycheck-select-checker
   :desc "Prettify"            "P" #'prettify-symbols-mode
   :desc "Code outline"        "o" #'lsp-ui-imenu
   :desc "Help (LSP)"          "h" #'lsp-describe-thing-at-point
   :desc "Help (Dash)"         "H" #'counsel-dash
   :desc "Changelog"           "l" #'change-log-find-file)
 (:prefix-map ("e" . "export")
   :desc "Markdown open"       "m" #'markdown-open
   :desc "Org HTML+Pandoc"     "h" #'org-pandoc-export-to-html5-and-open
   :desc "Org HTML"            "H" #'org-html-export-to-html
   :desc "Org PDF+Pandoc"      "p" #'org-pandoc-export-to-latex-pdf-and-open
   :desc "Org PDF+Latex"       "P" #'org-latex-export-to-pdf
   :desc "Org HTML publish"    "=" #'org-publish-all
   :desc "Org dispatcher"      "d" #'org-export-dispatch)
 (:prefix "g"
   :desc "Popup hunk"          "-" #'git-gutter:popup-hunk)
 (:prefix "n"
   :desc "Gist region/buffer"  "g" #'gist-region-or-buffer
   :desc "Markdown hide/show"  "M" #'markdown-toggle-markup-hiding
   :desc "Org narrow"          "N" #'org-narrow-to-subtree
   :desc "Interleave"          "I" #'interleave-mode
   :desc "Org clean results"   "r" #'org-remove-all-result-blocks
   :desc "Org sort entries"    "R" #'org-sort-entries
   :desc "Add bookmark"        "B" #'bookmark-set
   :desc "Select dictionary"   "Z" #'ispell-change-dictionary
   :desc "Insert URL"          "u" #'insert-url)
 (:prefix "o"
   :desc "Geiser REPL"         "G" #'geiser
   :desc "Github"              "g" #'open-github
   :desc "IELM"                "i" #'ielm
   :desc "IRC"                 "I" #'=irc
   :desc "Jupyter"             "J" #'jupyter-run-repl)
 (:prefix "t"
   :desc "Unfill region"       "U" #'unfill-region
   :desc "Select theme"        "t" #'counsel-load-theme
   :desc "Select dictionary"   "Z" #'ispell-change-dictionary
   :desc "Switch dictionary"   "z" #'ispell-cycle-dictionary
   :desc "Spellcheck"          "W" #'flyspell-buffer
   :desc "Keypression"         "k" #'keypression-mode
   :desc "Poly Markdown"       "P" #'poly-markdown-mode
   :desc "Toggle letter case"  "L" #'toggle-letter-case
   :desc "Show/Hide modeline"  "M" #'global-hide-mode-line-mode
   :desc "Beautify buffer"     "B" #'format-all-mode
   :desc "Auto fill"           "f" #'auto-fill-mode
   :desc "Switch theme"        "T" #'cycle-theme
   :desc "Frame maximized"     "F" #'toggle-frame-maximized  ;; instead of fullscreen
   :desc "Ruler"               "R" #'fci-mode
   :desc "Mail checker"        "n" #'mu4e-alert-enable-mode-line-display
   :desc "Undo tree"           "u" #'undo-tree-visualize
   :desc "Flycheck"            "c" #'flycheck-buffer)
 (:prefix "w"
   :desc "Swap window H/V"     "w" #'doom/window-layout-toggle))
