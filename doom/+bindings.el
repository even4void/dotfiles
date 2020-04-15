;;; ~/.config/doom/+bindings.el -*- lexical-binding: t; -*-

;; Other bindings are defined in ~/.emacs.d/modules/config/default;
;; See +evil-bindings.el for usual Vim-like bindings

(map!

 ;; dictionnary
 :n "C-c d"       #'osx-dictionary-search-word-at-point

 :n "s-<return>"  #'toggle-frame-fullscreen
 :n "gh"          #'lsp-describe-thing-at-point

 ;; osx shortcuts without cua-mode
 "s-z"        #'undo
 "s-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
 "s-v"        #'yank
 "s-s"        #'save-buffer
 "s-a"        #'mark-whole-buffer
 "s-b"        #'persp-switch-to-buffer
 "s-;"        #'eval-expression
 "s-r"        #'query-replace
 "s-p"        #'counsel-M-x
 "s-@"        #'neotree-toggle
 "s-<left>"    #'evil-window-left
 "s-<right>"   #'evil-window-right
 "s-<up>"      #'evil-window-up
 "s-<down>"    #'evil-window-down
 "C-s-<left>"  #'+workspace/switch-left
 "C-s-<right>"  #'+workspace/switch-right

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
     :n "C-<return>"  #'ess-eval-line))

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
 :desc "Next workspace"     ">"  #'+workspace:switch-next
 :desc "Ivy bibtex"         "B"  #'ivy-bibtex
 :desc "Split horizontally" "-"  #'split-window-below
 :desc "Split vertically"   "|"  #'split-window-right
 (:prefix "s"
   :desc "Ivy bibtex"          "B" #'ivy-bibtex
   :desc "Counsel Ag"          "c" #'doom/counsel-region-or-symbol
   :desc "Counsel Dash"        "C" #'counsel-dash-at-point
   :desc "Dictionary"          "w" #'osx-dictionary-search-pointer
   :desc "dictionary (input)"  "W" #'osx-dictionary-search-input
   :desc "Replace (regex)"     "R" #'replace-regexp)
 (:prefix "c"
   :desc "Available checkers"  "z" #'flycheck-verify-setup
   :desc "Select checker"      "y" #'flycheck-select-checker
   :desc "LSP diagnostics"     "L" #'lsp-describe-session
   :desc "Prettify"            "P" #'prettify-symbols-mode
   :desc "LSP start"           "S" #'+lsp-init-a
   :desc "LSP shutdown"        "q" #'lsp-workspace-shutdown
   :desc "Code outline"        "o" #'lsp-ui-imenu
   :desc "LSP doc mode"        "u" #'lsp-ui-doc-mode
   :desc "Rust hints mode"     "R" #'lsp-rust-analyzer-inlay-hints-mode
   :desc "Help (LSP)"          "h" #'lsp-describe-thing-at-point
   :desc "Help (Dash)"         "H" #'counsel-dash
   :desc "Changelog"           "l" #'change-log-find-file)
 (:prefix-map ("e" . "export")
   :desc "Markdown open"       "m" #'markdown-open
   :desc "Org HTML open"       "h" #'org-pandoc-export-to-html5-and-open
   :desc "Org PDF open"        "p" #'org-pandoc-export-to-latex-pdf-and-open
   :desc "Org HTML publish"    "H" #'org-publish-all
   :desc "Org dispatcher"      "d" #'org-export-dispatch)
 (:prefix "n"
   :desc "Gist region/buffer"  "g" #'gist-region-or-buffer
   :desc "Org narrow"          "N" #'org-narrow-to-subtree
   :desc "Interleave"          "I" #'interleave-mode
   :desc "Org clean results"   "r" #'org-remove-all-result-blocks
   :desc "Add bookmark"        "b" #'bookmark-set
   :desc "Insert URL"          "u" #'insert-url)
 (:prefix "o"
   :desc "Mu4e"                "M" #'mu4e
   :desc "Geiser REPL"         "G" #'geiser
   :desc "IELM"                "i" #'ielm
   :desc "Elfeed"              "F" #'elfeed
   :desc "IRC"                 "I" #'erc
   :desc "Toggle zsh popup"    "z" #'+term/toggle
   :desc "Open zsh here"       "Z" #'+term/here
   :desc "Deft"                "D" #'deft
   :desc "dictionary"          "w" #'osx-dictionary-search-pointer
   :desc "dictionary (input)"  "W" #'osx-dictionary-search-input)
 (:prefix "t"
   :desc "Unfill region"       "U" #'unfill-region
   :desc "Select theme"        "t" #'counsel-load-theme
   :desc "Select dictionary"   "Z" #'ispell-change-dictionary
   :desc "Spellcheck"          "W" #'flyspell-buffer
   :desc "Poly Markdown"       "P" #'poly-markdown-mode
   :desc "Toggle letter case"  "L" #'toggle-letter-case
   :desc "Show/Hide modeline"  "M" #'global-hide-mode-line-mode
   :desc "Beautify buffer"     "B" #'format-all-mode
   :desc "Auto fill"           "f" #'auto-fill-mode
   :desc "Ruler"               "R" #'fci-mode
   :desc "Undo tree"           "u" #'undo-tree-visualize
   :desc "Markdown hide/show"  "m" #'markdown-toggle-markup-hiding
   :desc "Flycheck"            "c" #'flycheck-buffer)
 (:prefix "w"
   :desc "Swap window H/V"     "w" #'doom/window-layout-toggle))
