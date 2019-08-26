;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press `SPC f e R' (Vim style) or
     ;; `M-m f e R' (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (osx :variables
          osx-right-option-as 'meta)
     helm
     (auto-completion :variables
                      auto-completion-enable-help-tooltip 'manual
                      auto-completion-enable-snippets-in-popup t)
     shell-scripts
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     (syntax-checking :variables
                      syntax-checking-enable-tooltips t)
     (ess :variables
          ess-enable-smart-equals nil
          ess-disable-underscore-assign nil)
     lsp
     (python :variables
             python-backend 'lsp
             python-formatter 'yapf
             python-enable-yapf-format-on-save t
             python-test-runner '(pytest nose)
             python-sort-imports-on-save t)
     emacs-lisp common-lisp racket scheme purescript
     (clojure :variables
              clojure-enable-fancify-symbols t
              clojure-enable-sayid t
              clojure-enable-clj-refactor t)
     (javascript :variables
                 javascript-backend 'lsp   ;; tern
                 javascript-fmt-tool 'web-beautify
                 js2-basic-offset 2
                 js-indent-level 2)
     (html :variables
           web-fmt-tool 'web-beautify) ;; 'prettier
     prettier yaml
     github
     (git :variables
          git-magit-status-fullscreen t
          git-gutter-use-fringe t
          git-enable-github-support t)
     (version-control :variables
                      version-control-diff-tool 'git-gutter
                      version-control-diff-side 'left
                      version-control-global-margin t)
     neotree
     (org :variables
          org-enable-github-support t
          org-enable-bootstrap-support t
          org-enable-hugo-support t
          org-enable-org-journal-support t
          org-projectile-file "todo.org")
     markdown pandoc pdf deft bibtex
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     enable-flyspell-auto-completion t)
     (latex :variables
            latex-enable-folding t)
     (mu4e :variables
           mu4e-enable-mode-line nil
           mu4e-use-maildirs-extension t
           mu4e-enable-async-operations t)
    )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(helpful doom-themes minions eshell-git-prompt focus)

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(vi-tilde-fringe)

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; File path pointing to emacs 27.1 executable compiled with support
   ;; for the portable dumper (this is currently the branch pdumper).
   ;; (default "emacs-27.0.50")
   dotspacemacs-emacs-pdumper-executable-file "emacs-27.0.50"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=~/.emacs.d/.cache/dumps/spacemacs.pdmp/
   ;; (default spacemacs.pdmp)
   dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default nil)
   dotspacemacs-verify-spacelpa-archives nil

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 5))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(doom-nord
                         doom-nord-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(doom)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Inziu Iosevka CL"
                               :size 14
                               :weight normal
                               :width normal)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.2

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols nil

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers '(:t
                               :disabled-for-modes
                                 dired-mode
                                 doc-view-mode
                                 pdf-view-mode)

   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%f"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(height . 38))
  (add-to-list 'default-frame-alist '(width . 145))
  (setq ns-use-proxy-icon nil)
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."
  (setq mac-option-modifier 'none)
  (set-face-italic 'font-lock-comment-face t)
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  (global-set-key (kbd "C-h F") #'helpful-function)
  (global-set-key (kbd "C-h C") #'helpful-command)
  (global-set-key (kbd "H-b") #'neotree-toggle)
  (global-set-key (kbd "H-;") #'evilnc-comment-operator)
  (global-set-key (kbd "H-:") #'evil-toggle-fold)
  (global-set-key (kbd "H-g") #'magit-status)
  (global-set-key (kbd "H-h") #'helpful-at-point)
  (global-set-key (kbd "H-u") #'capitalize-region)
  (global-set-key (kbd "H-o") #'org-capture)
  (global-set-key (kbd "H-t") #'split-window-vertically)
  (global-set-key (kbd "H-f") #'helm-find-files)
  (global-set-key (kbd "H-,") #'helm-swoop)
  (global-set-key (kbd "H-p") #'helm-projectile-ag)
  (global-set-key (kbd "H-r") #'query-replace)
  (spacemacs/set-leader-keys "so" 'helm-occur)
  (spacemacs/set-leader-keys "sz" 'helm-bibtex)
  (spacemacs/set-leader-keys "sv" 'org-journal-search)
  (spacemacs/set-leader-keys "s<" 'xref-find-definitions)
  (spacemacs/set-leader-keys "s>" 'xref-find-references)
  (spacemacs/set-leader-keys "aojS" 'org-journal-new-scheduled-entry)
  (spacemacs/set-leader-keys "aojV" 'org-journal-schedule-view)
  (spacemacs/set-leader-keys-for-major-mode 'org "bR" 'my/org-remove-all-result-blocks)
  (delete-selection-mode 1)
  (eshell-git-prompt-use-theme 'powerline)
  (setq pdf-info-epdfinfo-program "~/local/bin/epdfinfo"
        pdf-tools-handle-upgrades nil)
  ;; (setq helm-ff-skip-boring-files t)
  (setq flycheck-html-tidy-executable "/usr/local/bin/tidy")
  (with-eval-after-load 'web-mode
    (setq web-mode-markup-indent-offset 2
          web-mode-css-indent-offset 2
          web-mode-enable-current-column-highlight nil))
  (with-eval-after-load 'dired
    (setq dired-listing-switches "-alh")
    (define-key dired-mode-map (kbd "RET")
      'dired-find-alternate-file)
    (define-key dired-mode-map (kbd "^")
      (lambda () (interactive) (find-alternate-file ".."))))
  (doom-themes-org-config)
  (doom-themes-neotree-config)
  (setq doom-neotree-enable-variable-pitch nil
        doom-neotree-file-icons 'simple)
  (setq doom-modeline-buffer-file-name-style 'file-name ;; truncate-upto-project
        doom-modeline-major-mode-icon nil
        doom-modeline-persp-name nil
        doom-modeline-github t
        doom-modeline-enable-word-count t
        doom-modeline-version t
        doom-modeline-mu4e nil
        doom-modeline-minor-modes t)
  (minions-mode 1)
  (setq minions-mode-line-lighter "◎")
  ;; (spacemacs/toggle-vi-tilde-fringe-off)
  (setq neo-vc-integration '(face))
  ;; (setq pcomplete-ignore-case t)        ;; case insensitive path in eshell
  ;; (setq eshell-glob-case-insensitive t) ;; though it is not working as expected
  ;; (setq spacemacs-buffer--warnings nil)
  ;; git
  ;; http://manuel-uberti.github.io/emacs/2018/02/17/magit-bury-buffer/
  (defun my/magit-kill-buffers (param)
    "Restore window configuration and kill all Magit buffers."
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  ;; (validate-setq magit-bury-buffer-function #'my/magit-kill-buffers)
  (setq magit-repository-directories '(("~/git/" . 1)
                                       ("~/org/" . 1)))
  ;; FIXME This no longer works apparently...
  ;; (with-eval-after-load 'magit-org-todos
  ;;   (magit-org-todos-autoinsert))
  (with-eval-after-load 'magit
    (setq magit-revision-show-gravatars nil))
  (with-eval-after-load 'git-gutter-fringe
    ;; stolen from doom-emacs
    ;; https://bit.ly/2UQKkQA
    ;;(setq-default fringes-outside-margins t)
    (fringe-helper-define 'git-gutter-fr:added '(center repeated)
      "XXX.....")
    (fringe-helper-define 'git-gutter-fr:modified '(center repeated)
      "XXX.....")
    (fringe-helper-define 'git-gutter-fr:deleted '(center repeated)
      "XXX....."))
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'html-tidy 'web-mode)
    (setq flycheck-indication-mode 'right-fringe))
  ;; (show-paren-mode 1)
  (setq show-paren-style 'expression)
  (with-eval-after-load 'company
    (setq company-tooltip-limit 10
          company-minimum-prefix-length 2
          company-idle-delay 0.1
          ;; company-tooltip-minimum-width 60
          ;; company-tooltip-margin 0
          company-show-numbers nil
          company-tooltip-offset-display nil
          company-dabbrev-downcase nil
          company-dabbrev-ignore-case nil
          company-dabbrev-code-other-buffers t
          company-tooltip-align-annotations t
          ;; company-require-match 'never
          ;; company-frontends '(company-childframe-frontend company-echo-metadata-frontend)
          company-global-modes '(not comint-mode erc-mode message-mode help-mode gud-mode org-mode text-mode markdown-mode)
          company-childframe-child-frame nil))
  (setq time-stamp-active t
        time-stamp-line-limit 10)
  (add-hook 'write-file-functions 'time-stamp)
  (add-hook 'org-mode-hook #'turn-on-visual-line-mode)
  (add-hook 'text-mode-hook #'turn-on-visual-line-mode)
  (with-eval-after-load 'focus
    (setq focus-mode-to-thing '((prog-mode . defun) (text-mode . line))))
  (add-to-list 'auto-mode-alist '("\\.Rmd\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.rmd\\'" . markdown-mode))
  (setq markdown-open-command "/usr/local/bin/mark"
        markdown-command "/usr/local/bin/multimarkdown"
        markdown-enable-math t
        markdown-fontify-code-blocks-natively t
        markdown-hide-markup t
        markdown-gfm-uppercase-checkbox t
        markdown-header-scaling-values '(1.1 1.0 1.0 1.0 1.0 1.0))
  (defun my/pretty-symbols ()
    (setq prettify-symbols-alist
          '(("lambda" . 955)
            ("->" . 8594)
            ("=>" . 8658)
            ("map" . 8614)
            ("<=" . 8804)
            (">=" . 8805)
            ("<-" . 8592)
            ("!=" . 8800))))
  (add-hook 'clojure-mode-hook 'my/pretty-symbols)
  (add-hook 'racket-mode-hook 'my/pretty-symbols)
  (add-hook 'ess-mode-hook 'my/pretty-symbols)
  ;; doesn't seem to work with Python, but see
  ;; https://bit.ly/2Eir09n
  ;; (add-hook 'python-mode-hook 'my/pretty-symbols)
  (eval-after-load "python-mode"
    '(define-key python-mode-map (kbd "C-return") 'python-shell-send-region))
  ;; (add-hook 'prog-mode-hook #'highlight-symbol-mode)
  (defun my/comint-mode-hook ()
    (local-set-key '[up] 'comint-previous-input)
    (local-set-key '[down] 'comint-next-input))
  (add-hook 'inferior-ess-mode-hook 'my/comint-mode-hook)
  ;; ox-hugo
  (setq org-hugo-default-section-directory "micro"
        org-hugo-default-base-dir "~/Sites/aliquote")
  ;; agenda and calendar
  ;; (with-eval-after-load 'org-agenda
  ;;   (require 'org-projectile)
  ;;   (mapcar #'(lambda (file)
  ;;               (when (file-exists-p file)
  ;;                 (push file org-agenda-files)))
  ;;           (org-projectile-todo-files)))
  ;; deft
  (setq deft-extensions '("org" "md" "txt"))
  (setq deft-directory "~/Documents/Drafts")
  (setq deft-text-mode 'markdown-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-recursive t)
  ;; FIXME: need to (setq deft-strip-summary-regexp)
  (setq deft-file-naming-rules
        '((noslash . "-")
          (nospace . "-")
          (case-fn . downcase)))
  ;; Python
  (setq doom-modeline-python-executable "python3")
  (setq python-shell-interpreter "python3")
  (setq python-shell-interpreter-args "-m IPython --simple-prompt -i")
  (setq flycheck-python-pycompile-executable "python3"
        flycheck-python-pylint-executable "python3"
        flycheck-python-flake8-executable "python3")
  (add-hook 'inferior-python-mode-hook 'my/comint-mode-hook)
  ;; org
  (defun my/retrieve-url ()
    "Retrieve the URL of the current Safari page as a string."
    (org-trim (shell-command-to-string
               "osascript -e 'tell application \"Safari\" to return URL of document 1'")))
  (defun my/insert-url ()
    "Insert URL of current browser page into Emacs buffer."
    (interactive)
    (insert (my/retrieve-url)))
  (defun org-journal-find-location ()
    (org-journal-new-entry t)
    (goto-char (point-min)))
  ;; https://bit.ly/2FdnWf5
  (with-eval-after-load 'org
    (require 'ox-bibtex)
    (setq org-hide-emphasis-markers t
          org-babel-min-lines-for-block-output 1000
          org-startup-with-inline-images nil
          org-confirm-babel-evaluate nil
          org-src-fontify-natively t
          org-support-shift-select t
          org-src-tab-acts-natively nil
          org-html-htmlize-output-type nil
          org-html-validation-link nil
          org-journal-dir "~/org/_j/"
          org-journal-file-format "%Y-%m-%d"
          ;; org-journal-date-prefix "#+TITLE: "
          org-journal-date-format "%d/%m/%Y"
          ;; org-journal-time-prefix "* "
          org-journal-time-format "%Hh%M"
          org-journal-enable-agenda-integration t
          org-refile-targets (quote ((nil :maxlevel . 9)
                                     (org-agenda-files :maxlevel . 9)))
          org-icalendar-store-UID t
          org-icalendar-include-todo "all"
          org-icalendar-combined-agenda-file "~/org/org-journal.ics"
          org-default-notes-file "~/org/notes.org"
          org-default-todo-file "~/org/todos.org"
          org-agenda-files '("~/org/todos.org" "~/org/notes.org" "~/org/papers.org" "~/org/diary.org")
          org-bibtex-file "~/Documents/Drafts/references.bib"
          org-todo-keywords '((sequence "TODO" "STAR" "|" "DONE" "CANC"))
          org-latex-pdf-process '("latexmk -pdf -f -outdir=%o %f"))
    ;; (add-to-list 'org-agenda-files (expand-file-name "~/org"))
    ;; notes.org, todos.org and diary.org
    (setq org-capture-templates
          '(("b" "Blog post" entry (file+headline "micro.org" "Micro")
             "** TODO %?\n:PROPERTIES:\n:EXPORT_FILE_NAME:\n:END:\n%^g\n" :empty-lines 1)
            ("d" "Diary" entry (file+datetree "diary.org" "Diary")
             "* %?\n%T\n  %i\nFrom: %a")
            ("j" "Journal" entry (function org-journal-find-location)
             "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
            ("m" "Meetings" entry (file+headline "diary.org" "Meetings")
	           "** MEET with %? :MEETING:\n%t" :clock-in t :clock-resume t)
            ("n" "Notes" entry (file+headline org-default-notes-file "Notes")
             "* %u %?\n%i" :prepend t :kill-buffer t)
            ("p" "Projects" entry (file+headline org-default-todo-file "Projects")
             "* TODO %? %^g \n %i\n")
            ("t" "Todo" entry (file+headline org-default-todo-file "Tasks")
             "* TODO %? %^g \n %i\n")
            ("w" "URL" entry (file+headline org-default-notes-file "Inbox")
             "* %? \n%U\n%(my/retrieve-url)\n")))  ;; FIXME sort entries and add :prepend
    (setq org-babel-clojure-backend 'cider
          inferior-R-program-name "/usr/local/bin/R"
          inferior-STA-program-name "/usr/local/bin/stata-mp")
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((clojure . t)
       (python . t)
       (C . t)
       (R . t)
       (stata . t)
       (lisp . t)
       (emacs-lisp . t)))
    (defun my/org-remove-all-result-blocks ()
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward "#+begin_src " nil t)
          (org-babel-remove-result))))
    (defun my/org-mode-hook ()
      "Stop the org-level headers from increasing in height relative to the other text."
      (dolist (face '(org-level-1 org-level-2 org-level-3 org-level-4 org-level-5))
        (set-face-attribute face nil :weight 'semi-bold :height 1.0)))
    (add-hook 'org-mode-hook 'my/org-mode-hook)
    (defun my/org-pass-link-to-system (link)
      (if (string-match "^[a-zA-Z0-9\-]+:" link)
          (shell-command (concat "open " (shell-quote-argument link)))
        nil))
    (add-hook 'org-open-link-functions 'my/org-pass-link-to-system)
    (setq org-pandoc-options '((standalone . t)
                               (bibliography . "~/Documents/Drafts/references.bib")))
    ;; https://lists.gnu.org/archive/html/emacs-orgmode/2012-08/msg00934.html
    (defun my/org-remove-all-result-blocks ()
      (interactive)
      (save-excursion
        (goto-char (point-min))
        (while (search-forward "#+begin_src " nil t)
          (org-babel-remove-result)))))
    ;; bibtex
  (with-eval-after-load 'bibtex
    (setq bibtex-field-delimiters 'double-quotes
          bibtex-autokey-year-length 4
          bibtex-autokey-name-year-separator "-"
          bibtex-autokey-year-title-separator "-"
          bibtex-autokey-titleword-separator "-"
          bibtex-autokey-titlewords 2
          bibtex-autokey-titlewords-stretch 1
          bibtex-autokey-titleword-length 5))
  (with-eval-after-load 'org-ref
    (setq reftex-default-bibliography '("~/Documents/Drafts/references.bib")
          org-ref-bibliography-notes "~/org/papers.org"
          org-ref-default-bibliography '("~/Documents/Drafts/references.bib")
          org-ref-pdf-directory "~/Documents/Papers"
          org-ref-note-title-format "* [[/Users/chl/Documents/Papers/%k.pdf][%k]] - %t\n :PROPERTIES:\n :Custom_ID: %k\n :INTERLEAVE_PDF: /Users/chl/Documents/Papers/%k.pdf\n :END:\n"))
  (with-eval-after-load 'helm-bibtex
    (setq bibtex-completion-bibliography '("~/Documents/Drafts/references.bib")
          bibtex-completion-library-path '("~/Documents/Papers" "~/Documents/Papers/_iBooks")
          bibtex-completion-pdf-extension '(".pdf" ".epub")
          bibtex-completion-notes-path "~/org/papers.org"
          bibtex-completion-pdf-symbol ""
          bibtex-completion-notes-symbol "≣"
          ;; bibtex-completion-notes-symbol ""
          bibtex-completion-additional-search-fields '(keywords)
          bibtex-completion-pdf-open-function (lambda (fpath)
                                                (call-process "open" nil 0 nil "-a" "/Applications/Preview.app" fpath))
          bibtex-completion-display-formats
          '((t . "${author:30} ${title:*} ${year:4} ${=type=:8} ${=has-pdf=:1} ${=has-note=:1}"))
          bibtex-completion-format-citation-functions
          '((org-mode      . bibtex-completion-format-citation-org-link-to-PDF)
            (latex-mode    . bibtex-completion-format-citation-cite)
            (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
            (default       . bibtex-completion-format-citation-default))
          bibtex-completion-cite-prompt-for-optional-arguments nil
          bibtex-completion-notes-template-one-file
          (format "* [[/Users/chl/Documents/Papers/${=key=}.pdf][${=key=}]] - ${title}\n :PROPERTIES:\n :Custom_ID: ${=key=}\n :INTERLEAVE_PDF: /Users/chl/Documents/Papers/${=key=}.pdf\n :END:\n"))
    (advice-add 'bibtex-completion-candidates
                :filter-return 'reverse))
  ;; TeX
  (setq TeX-auto-save nil
        TeX-source-correlate-method 'synctex
        TeX-view-program-selection '((output-pdf "PDF Tools"))
        TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
        TeX-source-correlate-start-server t)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  ;; mu4e
  (with-eval-after-load 'mu4e
    (setq mu4e-get-mail-command "mbsync -a"
          smtpmail-stream-type 'starttls
          mu4e-change-filenames-when-moving t
          ; mu4e-maildirs-extension-custom-list '("archives" "icloud" "aliquote")
          mu4e-maildirs-extension-maildir-expanded-prefix "»"
          mu4e-maildirs-extension-maildir-default-prefix "◉"
          mu4e-maildirs-extension-toggle-maildir-key "+"
          mu4e-compose-format-flowed t
          mu4e-headers-show-threads nil
          mu4e-headers-date-format "%Y-%m-%d %H:%M"
          mu4e-confirm-quit nil
          mu4e-completing-read-function 'completing-read
          smtpmail-queue-dir "~/.mail/queue/cur"
          mu4e-maildir "~/.mail"
          mu4e-attachment-dir "~/Downloads")
    (setq mu4e-headers-fields
          '( (:date          .  25)    ;; instead of :human-date . 12
             (:flags         .   6)
             (:mailing-list  .  10)
             (:from          .  22)
             (:subject)))
    (setq mu4e-user-mail-address-list '("ch.lalanne@aliquote.org" "ch.lalanne@mac.com"))
    (setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "i ch.lalanne@mac.com"
           :enter-func (lambda () (mu4e-message "Enter ch.lalanne@mac.com context"))
           :leave-func (lambda () (mu4e-message "Leave ch.lalanne@mac.com context"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg :to "ch.lalanne@mac.com")))
           :vars '( ( user-mail-address      . "ch.lalanne@mac.com"  )
                    ( user-full-name         . "Christophe Lalanne" )
                    ( mu4e-sent-folder       . "/icloud/Sent Messages" )
                    ( mu4e-drafts-folder     . "/icloud/Drafts" )
                    ;; ( mu4e-trash-folder      . "/icloud/Trash" )
                    ( smtpmail-smtp-server   . "smtp.mail.me.com" )
                    ( smtpmail-stream-type   . starttls )
                    ( smtpmail-smtp-service  . 587 )
                    ( mu4e-compose-signature . (concat "chl\n"))))

         ,(make-mu4e-context
           :name "a ch.lalanne@aliquote.org"
           :enter-func (lambda () (mu4e-message "Enter ch.lalanne@aliquote.org context"))
           :match-func (lambda (msg)
                         (when msg
                           (mu4e-message-contact-field-matches msg :to "ch.lalanne@aliquote.org")))
           :vars '( ( user-mail-address       . "ch.lalanne@aliquote.org" )
                    ( user-full-name          . "Christophe Lalanne" )
                    ( mu4e-sent-folder        . "/aliquote/Sent" )
                    ( mu4e-drafts-folder      . "/aliquote/Drafts" )
                    ( mu4e-trash-folder       . "/aliquote/Trash" )
                    ( smtpmail-smtp-server    . "ssl0.ovh.net" )
                    ( smtpmail-smtp-service   . 587 )
                    ( mu4e-compose-signature  . (concat "chl\n")))) ))
    (setq mu4e-context-policy 'pick-first
          mu4e-compose-context-policy nil)
    (add-to-list 'mu4e-bookmarks
                  '("maildir:/aliquote/INBOX OR maildir:/icloud/INBOX" "All Inboxes" ?i))
    (add-to-list 'mu4e-bookmarks
                 '("maildir:/archives" "Archives" ?a)))

  ;; Helper macros
  (defun unfill-paragraph ()
    "Takes a multi-line paragraph and makes it into a single line of text."
    (interactive)
    (let ((fill-column (point-max)))
      (fill-paragraph nil)))
  (defun unfill-region ()
    "Unfill a region, i.e., make text in that region not wrap."
    (interactive)
    (let ((fill-column (point-max)))
      (fill-region (region-beginning) (region-end) nil)))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(defun dotspacemacs/emacs-custom-settings ()
  "Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization."
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-guide-delay 0.3)
 '(org-agenda-files nil)
 '(package-selected-packages
   '(focus eshell-git-prompt yaml-mode livid-mode skewer-mode json-navigator hierarchy json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc company-tern tern build-status stickyfunc-enhance srefactor sqlup-mode sql-indent sayid clj-refactor inflections edn multiple-cursors peg racket-mode ox-hugo org-download google-translate geiser ace-window sesman auctex swiper ess window-purpose lsp-mode ivy helm slime yasnippet-snippets yapfify xterm-color ws-butler writeroom-mode winum which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen utop use-package tuareg toml-mode toc-org tagedit symon swift-mode string-inflection spaceline-all-the-icons smeargle slime-company slim-mode shell-pop scss-mode sass-mode reveal-in-osx-finder restart-emacs rainbow-delimiters racer pyvenv pytest pyenv-mode py-isort pug-mode psci psc-ide prettier-js popwin pippel pipenv pip-requirements persp-mode pcre2el password-generator paradox pandoc-mode ox-twbs ox-pandoc ox-gfm overseer osx-trash osx-dictionary orgit org-ref org-projectile org-present org-pomodoro org-mime org-journal org-bullets org-brain open-junk-file ocp-indent neotree nameless multi-term mu4e-maildirs-extension mu4e-alert move-text mmm-mode minions markdown-toc magithub magit-svn magit-org-todos magit-gitflow macrostep lsp-ui lorem-ipsum live-py-mode link-hint launchctl julia-mode interleave insert-shebang indent-guide importmagic impatient-mode imenu-list hungry-delete hl-todo highlight-parentheses highlight-numbers highlight-indentation helpful helm-xref helm-themes helm-swoop helm-rtags helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-mu helm-mode-manager helm-make helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag google-c-style golden-ratio gnuplot gitignore-templates github-search github-clone gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gist gh-md fuzzy forge font-lock+ flyspell-popup flyspell-correct-helm flycheck-rust flycheck-rtags flycheck-pos-tip flycheck-ocaml flycheck-bashate flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu ess-R-data-view eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav editorconfig dumb-jump dotenv-mode doom-themes doom-modeline disaster diminish diff-hl deft cython-mode cquery counsel-projectile company-web company-statistics company-shell company-rtags company-quickhelp company-lsp company-c-headers company-auctex company-anaconda common-lisp-snippets column-enforce-mode clojure-snippets clean-aindent-mode clang-format cider-eval-sexp-fu cider centered-cursor-mode ccls cargo browse-at-remote auto-yasnippet auto-highlight-symbol auto-dictionary auto-complete-rst auto-compile auctex-latexmk aggressive-indent ace-link ace-jump-helm-line ac-ispell))
 '(safe-local-variable-values
   '((python-sort-imports-on-save)
     (javascript-backend . tern)
     (javascript-backend . lsp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
)
)
