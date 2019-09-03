;;; ~/.config/doom/+bindings.el -*- lexical-binding: t; -*-


;; Existing and most used bindings:
;; g c : toggle comment
;; za : toggle fold
(map!

 ;; Window movement
 :n "C-h" 'evil-window-left
 :n "C-j" 'evil-window-down
 :n "C-k" 'evil-window-up
 :n "C-l" 'evil-window-right

 :n "C-p" 'neotree-toggle

 ;; dictionnary
 :n "C-c d" 'osx-dictionary-search-word-at-point

 ;; osx shortcuts without cua-mode
 "s-z" #'undo
 "s-c" (if (featurep 'evil) #'evil-yank #'copy-region-as-kill)
 "s-v" #'yank
 "s-s" #'save-buffer
 "s-a" #'mark-whole-buffer
 "s-;" #'eval-expression

 (:map markdown-mode-map
   :i "s-i" #'markdown-insert-italic
   :i "s-b" #'markdown-insert-bold)

 (:after helpful
   (:map helpful-mode-map
     :n "RET"    #'helpful-visit-reference
     :n "o"      #'ace-link-help
     :n "q"      #'quit-window
     :n "Q"      #'ivy-resume))

 (:after osx-dictionary
   :map osx-dictionary-mode-map
   :n "q" #'osx-dictionary-quit
   :n "r" #'osx-dictionary-read-word
   :n "s" #'osx-dictionary-search-input
   :n "o" #'osx-dictionary-open-dictionary.app)

 ;; which-key items
 :leader
 (:prefix "/"
   :desc "Ivy bibtex"          "B" #'ivy-bibtex
   :desc "Counsel Ag"          "c" #'doom/counsel-region-or-symbol
   :desc "Counsel Dash"        "C" #'counsel-dash-at-point
   :desc "Dictionary"          "w" #'osx-dictionary-search-pointer
   :desc "dictionary (input)"  "W" #'osx-dictionary-search-input
   :desc "Swiper all"          "s" #'doom/swiper-all-region-or-symbol
   :desc "Replace"             "r" #'query-replace
   :desc "Replace (regex)"     "R" #'replace-regexp)
 (:prefix "c"
   :desc "Available checkers"  "z" #'flycheck-verify-setup
   :desc "Select checker"      "y" #'flycheck-select-checker
   :desc "LSP diagnostics"     "L" #'lsp-describe-session
   :desc "LSP shutdown"        "q" #'lsp-shutdown-workspace
   :desc "Code outline"        "o" #'lsp-ui-imenu
   :desc "Help (LSP)"          "h" #'lsp-describe-thing-at-point
   :desc "Help (Dash)"         "H" #'counsel-dash          ;; #'+lookup:dash
   :desc "Changelog"           "l" #'change-log-find-file)
 (:prefix "n"
   :desc "Ivy bibtex"          "B" #'ivy-bibtex
   :desc "Insert org citation" "c" #'org-ref-insert-cite-with-completion
   :desc "Unfill region"       "f" #'unfill-region
   :desc "Gist region/buffer"  "g" #'gist-region-or-buffer
   :desc "Org narrow"          "N" #'org-narrow-to-subtree
   :desc "Org clean results"   "r" #'org-remove-all-result-blocks
   :desc "Insert URL"          "u" #'insert-url)
 (:prefix "o"
   :desc "Mu4e"                "M" #'mu4e
   :desc "IELM"                "i" #'ielm
   :desc "IRC"                 "I" #'erc
   :desc "dictionary"          "w" #'osx-dictionary-search-pointer
   :desc "dictionary (input)"  "W" #'osx-dictionary-search-input)
 (:prefix "t"
   :desc "Select theme"        "t" #'counsel-load-theme
   :desc "Select dictionary"   "z" #'ispell-change-dictionary
   :desc "Spell check buffer"  "w" #'flyspell-buffer
   :desc "Markdown open"       "o" #'markdown-open
   :desc "Toggle letter case"  "L" #'toggle-letter-case
   :desc "Auto fill"           "f" #'auto-fill-mode
   :desc "Undo tree"           "u" #'undo-tree-visualize
   :desc "Markdown hide/show"  "m" #'markdown-toggle-markup-hiding
   :desc "Flycheck"            "c" #'flycheck-buffer)
 (:prefix "w"
   :desc "Swap window H/V"     "w" #'doom/window-layout-toggle))

;; ex
(evil-ex-define-cmd "wn" #'+workspace:new)
(evil-ex-define-cmd "wu" #'+workspace:rename)
