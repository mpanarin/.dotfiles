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
     helpful
     csv
     debug
     html
     (yaml :variables
           yaml-enable-lsp t)
     markdown
     (docker :variables
             docker-dockerfile-backend 'lsp)
     helm
     ranger
     prodigy
     better-defaults
     emacs-lisp
     (git :variables
          git-enable-magit-todos-plugin t)
     lsp
     dap
     (python :variables
             python-fill-column 80
             python-indent-offset 4
             python-backend 'lsp)
     django
     (elixir :variables
             elixir-backend 'lsp)
     phoenix
     erlang
     (sql :variables
          sql-capitalize-keywords t
          sql-capitalize-keywords-disable-interactive t
          sql-backend 'lsp
          sql-lsp-sqls-workspace-config-path 'root)
     (javascript :variables
                 javascript-backend 'lsp
                 javascript-fmt-tool 'web-beautify)
     react
     (typescript :variables
                 typescript-backend 'lsp)
     rust
     themes-megapack
     org
     (shell :variables
            shell-pop-autocd-to-working-dir nil
            shell-default-height 30
            shell-default-position 'bottom
            shell-default-shell 'vterm)
     spell-checking
     syntax-checking
     version-control
     (auto-completion :variables
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-snippets-in-popup t)
     (treemacs :variables
               treemacs-use-follow-mode t
               treemacs-use-filewatch-mode t
               treemacs-use-scope-type 'Perspectives)
     (ibuffer :variables
              ibuffer-group-buffers-by 'projects)
     pdf
     graphql
     osx
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages '(

;; General additional packages
                                      reverse-im                ;; allows usage shortcuts on russian keyboard
                                      srcery-theme              ;; theme
                                      exunit                    ;; elixir test runner
                                      autopair                  ;; autopairs quotes and brackets, used for snippets. TODO: should be considered to change to smartparens
                                      solaire-mode              ;; highlights test buffers with slightly brighter colors
                                      treemacs-magit            ;; magit integration
                                      treemacs-icons-dired      ;; use treemacs icons in dired
                                      magit-todos               ;; add TODOs and other keywords to the magit buffer
                                      python-pytest             ;; pytest runner
                                      centaur-tabs              ;; beautiful tabs for emacs, TODO: requires further fixing with spaceline flickering.
                                      nov                       ;; awesome epub mode
                                      calfw                     ;; great emacs calendar
                                      calfw-org                 ;; integration of calendar with org
                                      highlight-function-calls  ;; highlights function calls
                                      highlight-blocks          ;; highlights block, where cursor is
                                      coffee-mode               ;; mode for editing coffee-script files
                                      daemons                   ;; emacs UI for managing services from systemd and alike
                                      ox-reveal                 ;; add export to reveal.js from org
                                      org-fancy-priorities      ;; fancy priorities in org mode
                                      emojify                   ;; because dank
                                      mix                       ;; minor mode for mix files
                                      w3m
                                      pkgbuild-mode             ;; editing major mode for PKGBUILD files
                                      dash-functional
                                      frame-local
                                      exec-path-from-shell
                                      direnv

;; packages needed for dev with Cask
                                      ;; f
                                      ;; ecukes
                                      ;; shut-up
                                      ;; el-mock
                                      ;; ert-async
                                      ;; ert-runner
                                      )

   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '(doom-themes)

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(lsp-python-ms)

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

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 10

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

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
   dotspacemacs-editing-style 'vim

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 999

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((agenda . nil)
                                (projects . 3))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'org-mode

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(
                         doom-peacock
                         ;; solarized-light-high-contrast
                         ;; doom-snazzy
                         ;; srcery
                         ;; doom-molokai
                         ;; solarized-dark
                         spacemacs-light)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.3)
   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts.
   dotspacemacs-default-font '("Fira Code"
                               :size 13
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
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; TODO: check if this are still available
   ;; If non-nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil

   ;; TODO: check if this are still available
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t

   ;; TODO: check if this are still available
   ;; If non-nil, `J' and `K' move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text t

   ;; TODO: check if this are still available
   ;; If non-nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil

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
   dotspacemacs-auto-save-file-location 'original

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

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
   dotspacemacs-fullscreen-at-startup t

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native t

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 95

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
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers t

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
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
   dotspacemacs-frame-title-format "%t@%f"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode nil

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil))

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
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )


;; helpful small custom functions

(defun custom/kill-all-persp ()
  "Kills all perspectives with their buffers, except `Default'"
  (interactive)
  (let ((persps (seq-filter (lambda (persp) (not (equal persp "Default")))
                            (persp-names)))
        (persp-autokill-buffer-on-remove t))
    (if persps (persp-kill persps)))
  (spacemacs-buffer/refresh)
  (delete-other-windows))

(defun custom/insert-page-break ()
  (interactive)
  (evil-beginning-of-line)
  (insert "\C-l\n"))

(defun custom/toggle-breakpoint (trace)
  "Add a breakpoint and highlight it. TRACE is a string that should be pasted."
  (interactive)
  (let ((line (thing-at-point 'line)))
    (if (and line (string= trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert trace)
        (newline-and-indent)))))

(defun custom/helm-open-agenda-file ()
  (interactive)
  (helm
   :buffer "*Helm Open Agenda File*"
   :sources (helm-build-in-buffer-source "Agenda files:"
              :data (org-agenda-files)
              :fuzzy-match t
              :action '(("Visit file" . (lambda (candidate) (find-file candidate)))))))

(defun custom/re-seq (regexp string)
  "Get a list of all regexp matches in a string"
  (save-match-data
    (let ((pos 0)
          matches)
      (while (string-match regexp string pos)
        (push (match-string 0 string) matches)
        (setq pos (match-end 0)))
      (reverse matches))))

(defun custom/expand-region (start end separator)
  "Expand the region using certain SEPARATOR. Ex:
(a, b, c)
will turn into
(
  a,
  b,
  c,
)"
  (interactive
   (let ((string (read-string "Separator: " ",")))
     (list (region-beginning) (region-end) string)))

  (save-excursion
    (let ((text (buffer-substring-no-properties start end)))
      (delete-region start end)
      (let ((elems (mapcar #'string-trim (split-string text separator t))))
        (mapc (lambda (elem)
                (let ((elem (concat elem separator)))
                  (newline)
                  (insert  elem)
                  (back-to-indentation)
                  (indent-according-to-mode)
                  (right-char (length elem))))
              elems)
        (newline-and-indent)))))

(defun custom/format-on-save ()
  "On buffer save function triggering formatting for specific modes."
  (pcase major-mode
      ('elixir-mode (lsp-format-buffer))
    ))


;; Custom functions for spacemacs and modes customizations

(defun custom/evil-motions ()
  (evil-define-motion evil-last-non-blank (count)
    "Move the cursor to the last non-blank character
on the current line. If COUNT is given, move COUNT - 1
lines downward first."
    :type inclusive
    (evil-end-of-line count)
    (re-search-backward "^\\|[^[:space:]]")
    (setq evil-this-type (if (eolp) 'exclusive 'inclusive)))

  (define-key evil-motion-state-map "g$" 'evil-end-of-line)
  (define-key evil-motion-state-map "$" 'evil-last-non-blank)
  (define-key evil-motion-state-map "gb" 'evil-jump-backward)
  )

(defun custom/generic-improvements ()
  "Generic improvements and packages that are either too small, or not fitting other categories."
  (add-hook 'focus-out-hook #'garbage-collect)   ;; garbage-collect on focus out. Increases snappiness
  (fringe-mode '(16 . 8))                        ;; increase the left fringe width
  (setq auto-save-no-message t                   ;; no messages on autosaving please (27.1)
        spacemacs-buffer--current-note-type nil  ;; remove note from home buffer
        enable-local-variables :all              ;; allow unsafe vars in dir-locals
        enable-local-eval t                      ;; allow evals in dir-locals
        )

  (use-package reverse-im  ;; allow usage of russian keyboard
    :demand
    :config
    (reverse-im-activate "russian-computer"))
  (use-package nov  ;; associate epub file with nov-mode
    :defer t
    :mode ("\\.epub\\'" . nov-mode)
    :custom
    (nov-text-width 120)
    :bind
    (:map nov-mode-map
	        ("C-j" . nov-next-document)
	        ("C-k" . nov-previous-document)
	        ("C-l" . nov-goto-toc)))
  (use-package writeroom-mode  ;; configurate writeroom
    :commands (writeroom-mode writeroom--enable writeroom--disable)
    :defer t
    :custom
    (writeroom-width 125))
  (use-package solaire-mode  ;; enable Jolly Cooperation everywhere
    :demand
    :init
    (solaire-global-mode +1))
  (use-package web-mode  ;; configure webmode
    :defer t
    :mode "\\.mako\\'"
    :custom
    (web-mode-markup-indent-offset 2))
  (use-package csv-mode  ;; Make csv open always aligned with delimiters
    :defer t
    :hook (csv-mode . (lambda () (csv-toggle-invisibility) (csv-align-fields nil 1 (point-max))))  ;; TODO: this probably can be done better
    )
  (use-package smartparens  ;; autopair stuff in snippets and org
    :defer t
    :hook (org-mode . smartparens-mode)
    )
  (use-package yasnippet
    :defer t
    :config
    (add-hook 'yas-before-expand-snippet-hook (lambda () (autopair-mode 1)))
    (add-hook 'yas-after-exit-snippet-hook (lambda () (autopair-mode -1)))
    )
  (use-package pdf-tools
    :defer t
    :custom
    (pdf-view-display-size 'fit-page))
  (use-package sgml-mode  ;; used in rjsx for editing of jsx bits.
    :defer t
    :custom
    (sgml-basic-offset 4))
  (use-package vterm
    :defer t
    :config
    (defun vterm-send-ctrl-d ()
      "Sends `C-d' to the libvterm."
      (interactive)
      (vterm-send-key "d" nil nil t))
    :bind
    (:map vterm-mode-map
          ("C-c C-d" . vterm-send-ctrl-d)
          ("C-c C-x" . vterm-send-C-x)))
  (use-package flycheck
    :defer t
    :custom
    (flycheck-display-errors-delay 0.3))
  (use-package evil-surround
    :defer t
    :config
    (advice-add 'evil-surround-region :after (lambda (&rest args) (execute-kbd-macro "gv") (evil-forward-char))))  ;; FIXME: this interferes with "c s" surrounding
  (use-package ibuffer
    :defer t
    :config
    (defun custom-ibuffer-close-on-select (&rest args)
      (kill-buffer "*Ibuffer*"))
    (advice-add 'ibuffer-visit-buffer :after #'custom-ibuffer-close-on-select)
    :bind
    (:map evil-normal-state-local-map
          ("SPC b b" . ibuffer)))
  (use-package calendar  ;; TODO: probably should be dropped. Don't really use it anymore
    :defer t
    :custom
    (calendar-week-start-day 1))
  (use-package mmm-mode
    :defer t
    :init (remove-hook 'markdown-mode-hook 'spacemacs/activate-mmm-mode)) ;; please, no. see https://github.com/syl20bnr/spacemacs/issues/11790
  (use-package emojify
    :defer t
    :hook ((magit-mode) . emojify-mode))
  (use-package persp-mode
    :defer t
    :custom
    (persp-autokill-buffer-on-remove t))
  (use-package direnv
    :config
    (direnv-mode))
  (use-package all-the-icons)
  )

(defun custom/ligatures ()
  "Add fira code ligatures"
  ;; disable ligatures in helm, as they effectively hang emacs
  ;; TODO: check what the hell is this issue with helm
  ;; almost certainly he drops into infinite recursion as
  ;; CPU usage increases to 100%
  ;; TODO: something breaks eval-expression buffer
  ;; I should find a way that will disable ligatures in minibuffers altogether
  (add-hook 'helm-major-mode-hook
            (lambda ()
              (setq auto-composition-mode nil)))
  (add-hook 'ediff-mode-hook
            (lambda ()
              (setq auto-composition-mode nil)))
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq auto-composition-mode nil)))
  (add-hook 'eldoc-mode-hook
            (lambda ()
              (if (minibufferp) (setq auto-composition-mode nil))))
  (let ((alist '(
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")  ;; breaks ediff
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")  ;; breaks helm

                 (33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring])))))

(defun custom/unbind-useless-shit()
  (let ((keys '(

                ;;general
                "SPC \""                ;; Remove strange call to terminal here
                "SPC *"                 ;; Remove search in project, I use `SPC s p`
                "SPC /"                 ;; Remove search in project, I use `SPC s P`
                "SPC ;"                 ;; Remove this commenting, I use `g c` in visual
                "SPC ?"                 ;; useless help
                "SPC ²"                 ;; useless select of window
                "SPC `"                 ;; useless select of window
                "SPC u"                 ;; doubt that I have ever used this universal arg
                "SPC <f1>"              ;; what is even helm apropos?

                ;; applications
                "SPC a '"               ;; I use `SPC '` no need for this
                "SPC a k"               ;; Don't know what paradox is, don't care
                "SPC a u"               ;; Undo-tree-visualize is not even working
                "SPC a P"               ;; I don't use `proced`
                "SPC a Y"               ;; I don't use easy pg
                )))
    (mapc (lambda (key) (unbind-key key 'evil-normal-state-local-map)) keys))
  )

(defun custom/add-hooks ()
  "This is all the hooks I use"
  (add-hook 'python-mode-hook '(lambda () (display-fill-column-indicator-mode 1)))  ;; Add a line on 80 symbols
  (add-hook 'vterm-mode-hook (lambda () (setq-local evil-move-cursor-back nil)))    ;; Make cursor fixed in vterm
  (add-hook 'before-save-hook #'custom/format-on-save)                              ;; Add auto-formatting after save
  )

(defun custom/spacemacs-improvements ()
  "Several fixes from spacemacs issues"
  ;; TODO: switch to use-package here
  (with-eval-after-load 'spaceline    ;; Disable useless parts of spaceline
    (spaceline-toggle-minor-modes-off)
    (spaceline-toggle-purpose-off))

  (setq auto-save-mode nil            ;; switch to a better autosave mode. at least in OSX
        auto-save-default nil         ;; switch to a better autosave mode
        auto-save-visited-mode t      ;; switch to a better autosave mode
        auto-save-visited-interval 5  ;; Make auto-save more frequent and less painful
        create-lockfiles nil          ;; No lock files plz
        vc-follow-symlinks t          ;; Always follow symlinks pls
        )
  )

(defun custom/dap-generic ()
  "Generic LSP dap changes"

  (define-minor-mode custom-debug-mode
    "Remap some keybinds, specific to python and dap"
    :global nil

    (defun custom/toggle-breakpoint-generic ()
      (interactive)
      (pcase major-mode
        ('python-mode (spacemacs/python-toggle-breakpoint))
        ('elixir-mode (custom/toggle-breakpoint "require IEx; IEx.pry"))
        ('rjsx-mode (custom/toggle-breakpoint "debugger;")))
      )

      (spacemacs/set-leader-keys-for-minor-mode 'custom-debug-mode (kbd "d b d") 'dap-breakpoint-toggle)
      (spacemacs/set-leader-keys-for-minor-mode 'custom-debug-mode (kbd "d b b") 'custom/toggle-breakpoint-generic))
  (add-hook 'lsp-mode-hook (lambda () (custom-debug-mode t))))

(defun custom/centaur-tabs ()
  "Not currently used"
  (use-package centaur-tabs
    :demand
    :config
    (centaur-tabs-mode t)
    (setq centaur-tabs-style "bar")
    (setq centaur-tabs-height 32)
    (setq centaur-tabs-set-icons t)
    (setq centaur-tabs-set-bar 'over)
    (setq centaur-tabs-set-close-button nil)
    (setq centaur-tabs-cycle-scope 'tabs)
    (centaur-tabs-group-by-projectile-project)
    (defun centaur-tabs-hide-tab (x)
      (let ((name (format "%s" x)))
	      (or
         (window-dedicated-p (selected-window))
	       (string-prefix-p "*" name)
	       (string-prefix-p "magit" name)
	       )))
    :hook (
     (dashboard-mode . centaur-tabs-local-mode)
     (treemacs-mode . centaur-tabs-local-mode)
     (spacemacs-buffer-mode . centaur-tabs-local-mode)
     (term-mode . centaur-tabs-local-mode)
     (calendar-mode . centaur-tabs-local-mode)
     (org-agenda-mode . centaur-tabs-local-mode)
     (helpful-mode . centaur-tabs-local-mode)
     (dired-mode . centaur-tabs-local-mode)
     (zone-mode . centaur-tabs-local-mode)
     (helm-mode . centaur-tabs-local-mode))
    :bind
    ("C-<prior>" . centaur-tabs-backward)
    ("C-<next>" . centaur-tabs-forward)
    ("C-c t" . centaur-tabs-counsel-switch-group)
    (:map evil-normal-state-map
	        ("g l" . centaur-tabs-forward)
	        ("g h" . centaur-tabs-backward)
		)
    )
  )

(defun custom/tab-line-mode ()
  (load "~/.dotfiles/tab-line-custom"))

(defun custom/lsp-generic ()
  "Generic LSP changes"

  (use-package lsp-mode
    ;; :load-path "/home/m-panarin/projects/personal/elisp/lsp-mode/"  ;; when custom load is needed
    :defer t
    :commands lsp
    :ensure t
    :diminish lsp-mode
    :custom

    ;; General
    (lsp-file-watch-threshold nil)            ;; always filewatch
    (lsp-headerline-breadcrumb-enable t)      ;; show breadcrumbs
    (lsp-modeline-code-actions-enable t)      ;; show if codeactions available
    (lsp-eldoc-enable-hover nil)              ;; do not show hover info in eldoc, I have lsp-ui-doc for that
    (lsp-print-io t)                          ;; no logs, they make js lag like a little bitch
    (lsp-prefer-capf t)                       ;; try capf integration
    (lsp-signature-render-documentation nil)  ;; do not include docs in signature
    (lsp-modeline-diagnostics-enable nil)     ;; disable diagnostics in modeline, they are already present in spacemacs
    (lsp-modeline-code-actions-enable nil)    ;; disable code actions in modeline, they are already present in spacemacs
    (lsp-lens-enable t)                       ;; enable lenses if server supports them
    (lsp-headerline-arrow "‣")                ;; change the breadcrumbs separator

    ;; PYLS configs
    (lsp-pyls-plugins-rope-completion-enabled nil)             ;; disable garbage rope completion in pyls
    (lsp-pyls-plugins-jedi-completion-include-params nil)      ;; disable params in jedi completion, they are mediocre
    (lsp-pyls-plugins-pylint-enabled t)                        ;; enable pylint by deafult
    (lsp-pyls-plugins-flake8-enabled t)                        ;; enable flake8 by default
    (lsp-pyls-plugins-flake8-filename ["~/.dotfiles/flake8"])  ;; set the default flake8 config
    (lsp-pyls-plugins-pyflakes-enabled nil)                    ;; disable pyflakes by default
    (lsp-pyls-plugins-mccabe-enabled nil)                      ;; disable mccabe by default
    (lsp-pyls-plugins-pycodestyle-enabled nil)                 ;; disable pycodestyle by default
    (lsp-pyls-plugins-pydocstyle-enabled nil)                  ;; disable pydocstyle by default
    (lsp-pyls-plugins-jedi-completion-fuzzy nil)               ;; fuzzy off
    (lsp-pyls-rename-backend 'rope)                            ;; rename to rope

    ;; SQLS configs
    (lsp-sqls-server "~/go/bin/sqls")   ;; path to language server

    :config
    ;; Hacky way to update the var
    (mapc (lambda (val) (add-to-list 'lsp-file-watch-ignored-directories val))
          '("[/\\\\]_build\\'" "[/\\\\]deps\\'" "[/\\\\].elixir_ls\\'"))
    )
  (use-package lsp-ui
    :defer t
    :config
      ;; Use lsp-ui-peek instead of xref, as xref + lsp in emacs27 is broken
      (spacemacs/set-leader-keys-for-minor-mode 'lsp-ui-mode (kbd "g d") 'lsp-ui-peek-find-definitions)
      (spacemacs/set-leader-keys-for-minor-mode 'lsp-ui-mode (kbd "g r") 'lsp-ui-peek-find-references)
      (spacemacs/set-leader-keys-for-minor-mode 'lsp-ui-mode (kbd "g i") 'lsp-ui-peek-find-implementation)
    :custom

    ;; LSP-UI-DOC
    (lsp-ui-doc-position 'top)        ;; always keep doc at the top
    (lsp-ui-doc-include-signature t)  ;; add function signature to the buffer

    ;; LSP-UI-SIDELINE
    (lsp-ui-sideline-show-hover nil)         ;; do not show hover info, I have lsp-ui-doc for that
    (lsp-ui-sideline-show-code-actions nil)  ;; disable code actions as they are pretty lame

    ;; LSP-UI-PEEK
    (lsp-ui-peek-fontify 'always))  ;; always use fontify, otherwise highlight is broken in the left half
  )

(defun custom/python-specific ()
  "Changes specific to python-mode"
  (use-package python-pytest
    :defer t
    :after python
    :bind (:map spacemacs-python-mode-map (("t" . python-pytest-popup)))  ;; add pytest keybinds
    :custom
    (python-pytest-arguments '("--color" "--cov"))
    :config
    (magit-define-popup-switch 'python-pytest-popup ?C "Coverage" "--cov")
    )
  )

(defun custom/elixir-specific ()
  "Changes specific to elixir-mode"
  (use-package elixir-mode
    :defer t
    :config
    (add-hook 'elixir-mode-hook '(lambda () (progn
                                              (setq-local fill-column 98)
                                              (display-fill-column-indicator-mode 1))))
    (spacemacs/declare-prefix-for-mode 'elixir-mode (kbd "m t") "tests" "testing related functionality")
    (spacemacs/set-leader-keys-for-major-mode 'elixir-mode
      (kbd "t b") 'exunit-verify-all
      (kbd "t a") 'exunit-verify
      (kbd "t k") 'exunit-rerun
      (kbd "t t") 'exunit-verify-single))

  ;; TODO: this works but breaks lsp checker completely
  ;; see https://github.com/flycheck/flycheck/issues/1762
  ;; (add-hook 'lsp-mode-hook  (lambda ()
  ;;                            (if (eq major-mode 'elixir-mode)
  ;;                                (setq-local lsp-flycheck-live-reporting nil))))
  )

(defun custom/elisp-specific ()
  "Changes specific to emacs-lisp-mode"
  (spacemacs/set-leader-keys-for-major-mode 'emacs-lisp-mode (kbd "l") 'custom/insert-page-break)  ;; handy key to insert page-breaks

  (use-package highlight-function-calls
    :defer t
    :hook (emacs-lisp-mode . highlight-function-calls-mode))  ;; functions highlighter in elisp mode
  (use-package highlight-blocks
    :defer t
    :custom
    (highlight-blocks-max-innermost-block-count 1)
    :hook (emacs-lisp-mode . highlight-blocks-mode)
    )
  )

(defun custom/sql-specific ()
  "Changes specific to sql-mode"
  (spacemacs/set-leader-keys-for-major-mode 'sql-interactive-mode (kbd "la") 'sql-list-all)
  (spacemacs/set-leader-keys-for-major-mode 'sql-interactive-mode (kbd "lt") 'sql-list-table)
  (use-package sql
    :defer t
    :custom
    (sql-postgres-program "psql")
    (sql-postgres-options '("-P" "pager=off"))
    )
  )

(defun custom/magit-specific ()
  "Specific changes to magit and its subpackages"
  (use-package magit
    :defer t
    :config
    (define-key magit-diff-section-base-map [remap magit-visit-thing] 'magit-diff-visit-file-other-window)
    (define-key magit-diff-section-base-map (kbd "C-RET") 'magit-diff-visit-file)
    )
  (use-package magit-todos
    :defer t
    :hook (magit-mode . magit-todos-mode)
    :custom
    (magit-todos-update 60)
    (magit-todos-group-by '(magit-todos-item-filename magit-todos-item-keyword))
    :config
    (setq magit-todos-keywords 'hl-todo-keyword-faces)  ;; set magit todos faces
    :bind
    (:map magit-todos-section-map
          (("j" . evil-next-visual-line)
           ("l" . evil-previous-visual-line))
     :map magit-todos-item-section-map
          (("j" . evil-next-visual-line)
           ("l" . evil-previous-visual-line))))
  (use-package git-gutter+
    :defer t
    :config
    (fringe-helper-define 'git-gutter-fr+-added nil
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX.")
    (fringe-helper-define 'git-gutter-fr+-modified nil
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX.")
    (fringe-helper-define 'git-gutter-fr+-deleted nil
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX."
      "..XXXXX.")
    )
  )

(defun custom/org-specific ()
  "Changes specific to org-mode"
  (use-package org
    :defer t
    :custom
    (org-hide-emphasis-markers t)  ;; Autohide markup elements
    (org-pretty-entities t)        ;; add pretty entities
    (org-log-into-drawer t)        ;; log state changes to drawer
    (org-agenda-files (append      ;; add agenda files
                       (file-expand-wildcards "~/projects/EVO/evo.org")
                       (file-expand-wildcards "~/org/*.org")))
    :bind
    (:map org-mode-map
          ("RET" . newline-and-indent))
    :init
    (require 'ox-reveal))
  (use-package helm-org-rifle
    :defer t
    :bind
    (:map evil-normal-state-local-map
          ("SPC a o R" . helm-org-rifle-agenda-files)))
  (use-package calfw-org
    :defer t
    :bind
    (:map cfw:calendar-mode-map
          (("C-j" . cfw:navi-next-item-command)
           ("C-k" . cfw:navi-prev-item-command)
           ("RET" . cfw:org-open-agenda-day)
           ("A" . org-agenda-list))
    :map evil-normal-state-local-map
          ("SPC a o a" . cfw:open-org-calendar)))
  (use-package org-present
    :defer t
    :config
    (add-hook 'org-present-mode-hook (lambda ()
                                       (setq-local global-hl-line-mode nil)
                                       (setq display-line-numbers nil)
                                       (writeroom--enable)))
    (add-hook 'org-present-mode-quit-hook (lambda ()
                                            (setq-local global-hl-line-mode t)
                                            (setq display-line-numbers t)
                                            (writeroom--disable))))
  ;; TODO: maybe it is present in some layer. Check it
  ;; (use-package org-re-reveal
  ;;   :after org
  ;;   :demand t)
  (use-package org-fancy-priorities  ;; fancy org priorities
    :defer t
    :hook
    (org-mode . org-fancy-priorities-mode)
    :config
    (setq org-fancy-priorities-list '((?A . "❗")
                                      (?B . "⬆")
                                      (?C . "⬇"))))
  )

(defun custom/markdown-specific ()
  "Changes specific to markdown-mode"
  (use-package markdown-mode
    :mode (("README\\.md\\'" . gfm-mode))
    :defer t
    :custom
    (markdown-hide-markup t)  ;; Always hide markup in markdown-mode
    (markdown-hide-urls t)    ;; Hide and shorten URLs in markdown
    ))

(defun custom/treemacs-specific ()
  "Changes specific to treemacs-mode"
  (use-package treemacs
    :defer t
    :config
    (treemacs-git-mode 'deferred)  ;; Treemacs use deferred git-mode
    (treemacs-toggle-show-dotfiles)  ;; Hide dotfiles by default
    (add-to-list 'treemacs-ignored-file-predicates  ;; Ignore *.pyc files
                 (lambda (filename filepath)
                   (string-match-p "\.pyc$" filename)))
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)  ;; autohide files ignored by git please
    (define-key treemacs-mode-map (kbd "p") treemacs-project-map)  ;; set project map to 'p' as binding maps to maps is not working in :bind :\
    (define-key treemacs-mode-map (kbd "M-w") treemacs-workspace-map)  ;; set workspace map to 'W' as binding maps to maps is not working in :bind :\
    :custom
    (treemacs-lock-width 1)  ;; keep the width locked
    :bind
    (:map treemacs-mode-map
          ;; Revert navigation changes
          ("<treemacs-state> h" . treemacs-root-up)
          ("<treemacs-state> l" . treemacs-root-down)
          ;; Swap treemacs horizontal/vertical ace
          ("o a h" . treemacs-visit-node-ace-vertical-split)
          ("o a v" . treemacs-visit-node-ace-horizontal-split)))
  (use-package treemacs-icons-dired  ;; Show treemacs icons in dired
    :defer t
    :hook (dired-mode . treemacs-icons-dired-mode)))

(defun custom/helm-specific ()
  "Changes specific to helm-mode"
  (use-package helm
    :demand
    :bind
    (:map helm-map
          ;; Helm please. Allow me to move cursor normally
          ("<left>" . backward-char)
          ("<right>" . forward-char)))
  (use-package helm-ag
    :demand
    :bind
    (:map helm-ag-map
          ;; Helm-ag please. Allow me to move cursor normally
          ("<left>" . backward-char)
          ("<right>" . forward-char))))

(defun custom/generic-define-keys ()
  "Generic key defines I use, that are not tied to some specific mode,
   or mode I rarely use."
  (define-key global-map (kbd "<menu>") nil)                                                          ;; Unbind annoying sticky M-x on <menu>
  ;; Swap safe revert buffer and persp remove buffer
  (define-key evil-normal-state-local-map (kbd "SPC b r") 'spacemacs/safe-revert-buffer)
  (define-key evil-normal-state-local-map (kbd "SPC b R") 'persp-remove-buffer)
  (define-key evil-normal-state-local-map (kbd "SPC b y") 'spacemacs/copy-whole-buffer-to-clipboard)  ;; Bind copy whole buffer to lowercase y (whatafaqerino)
  (define-key evil-normal-state-local-map (kbd "SPC b k") 'custom/kill-all-persp)                     ;; Bind kill-all-persp
  (define-key evil-normal-state-local-map (kbd "SPC a o f") 'custom/helm-open-agenda-file)            ;; Bind open agenda file
  (define-key evil-normal-state-local-map (kbd "SPC x x") 'custom/expand-region)                      ;; Bind expand-region
  )

(defun custom/zoning ()
  "Changes specific to zoning"
  (use-package zone
    :defer t
    :config
    (zone-when-idle 240)
    :custom
    (zone-timer nil)
    (zone-programs [
                    zone-pgm-jitter
                    zone-pgm-putz-with-case
                    ;; zone-pgm-dissolve
                    ;; zone-pgm-explode
                    zone-pgm-whack-chars
                    zone-pgm-rotate-LR-variable
                    ;; zone-pgm-rotate-RL-variable
                    zone-pgm-drip
                    ;; zone-pgm-five-oclock-swan-dive
                    ;; zone-pgm-martini-swan-dive
                    ;; zone-pgm-rat-race
                    ;; zone-pgm-stress
                    ;; zone-pgm-stress-destress
                    ;; zone-pgm-random-life
                    ]))
  )

;; faces

(defun custom/extend-face-group (group)
  "Extend the whole `group' of faces with with :extend t"
  (mapc (lambda (face-pair) (if (eq (cadr face-pair) 'custom-face)
                                (set-face-attribute (car face-pair) nil :extend t)))
        (custom-group-members group nil)))

(defun custom/faces-set-extend-27 ()
  "Add extend param to faces, as it is nil by default in emacs 27"
  (with-demoted-errors "Error in faces: %s"
    (set-face-attribute 'region nil :extend t)
    (set-face-attribute 'highlight nil :extend t)
    (set-face-attribute 'hl-line nil :extend t)

    (custom/extend-face-group 'helm-faces)
    (custom/extend-face-group 'helm-swoop)
    (custom/extend-face-group 'magit-faces)
    (custom/extend-face-group 'org-faces)
    (custom/extend-face-group 'markdown-faces)
    (custom/extend-face-group 'rst-faces)
    (with-eval-after-load 'highlight-blocks
      (custom/extend-face-group 'highlight-blocks-faces))
    )
  )

(defun custom/faces-all ()
  (custom-set-faces
   ;; all
   ;; No wavy flycheck, please
   '(flycheck-error ((t (:underline "#e74c3c"))))
   '(flycheck-info ((t (:underline "#b6e63e"))))
   '(flycheck-warning ((t (:underline "#e2c770"))))
   '(flyspell-duplicate ((t (:underline "DarkOrange"))))
   '(flyspell-incorrect ((t (:underline "#e74c3c"))))
   )
  (setq hl-todo-keyword-faces
    '(("TODO" . "#dc752f")
      ("NEXT" . "#dc752f")
      ("THEM" . "#2d9574")
      ("PROG" . "#4f97d7")
      ("OKAY" . "#4f97d7")
      ("DONT" . "#f2241f")
      ("FAIL" . "#f2241f")
      ("DONE" . "#86dc2f")
      ("NOTE" . "#b1951d")
      ("KLUDGE" . "#b1951d")
      ("HACK" . "#b1951d")
      ("TEMP" . "#b1951d")
      ("FIXME" . "#f2241f")
      ("DEPRECATE" . "#f2241f")
      ("DEBUG" . "#dc752f")
      ("XXX" . "#dc752f")
      ("XXXX" . "#dc752f")))
  )

(defun custom/faces-snazzy ()
  (custom-set-faces
   ;; snazzy
   ;; Highlight yellow is used on python and elixir debugging lines, they should be readable
   '(hi-yellow ((t (:background "#e2c770" :foreground "black"))))
   ;; The line should be less annoying
   '(hl-line ((t (:background "gray17"))))
   ;; Lsp pick should be a bit prettier :3
   '(lsp-ui-peek-highlight ((t (:background "white" :distant-foreground "black" :foreground "black" :box (:line-width -1 :color "white")))))
   '(lsp-ui-peek-line-number ((t nil)))
   '(lsp-ui-peek-list ((t (:background "#031A25"))))
   )
  )

(defun custom/faces-doom-peacock ()
  (custom-set-faces
   ;; Lsp pick should be a bit prettier :3
   '(lsp-ui-peek-highlight ((t (:inherit lsp-ui-peek-header :background "#484745" :foreground "gray" :box 1))))
   ;; type and variable definition should be a bit more visible
   '(font-lock-type-face ((t (:foreground "#ff5d38"))))
   '(font-lock-variable-name-face ((t (:foreground "#ff5d38"))))
   '(hi-yellow ((t (:background "dark orange" :foreground "black" :weight bold))))
   ;; The line should be a bit more visible
   '(hl-line ((t (:background "gray19"))))

   ;; org levels
   '(org-level-1 ((t (:inherit outline-1 :height 1.3))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.2))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.15))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
   '(org-level-5 ((t (:inherit outline-5 :height 0.8))))
   )
  )

(defun custom/faces ()
  "Customized faces for different themes"
  ;; TODO: add faces for solair mode, no need for doom themes
  (custom/faces-all)

  (if (string= spacemacs--cur-theme "doom-snazzy")
      (custom/faces-snazzy))

  (if (string= spacemacs--cur-theme "doom-peacock")
      (custom/faces-doom-peacock))

  (custom/faces-set-extend-27)
  )



(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (exec-path-from-shell-initialize) ;; This is required in OSX as variables are not getting loaded from .zshrc
  (custom/unbind-useless-shit)

  (custom/evil-motions)
  (custom/generic-improvements)
  (custom/ligatures)

  (custom/add-hooks)

  (custom/spacemacs-improvements)
  (custom/generic-define-keys)

  (custom/lsp-generic)
  (custom/dap-generic)

  ;; (custom/centaur-tabs)
  (custom/tab-line-mode)

  (custom/python-specific)
  (custom/elixir-specific)
  (custom/elisp-specific)
  (custom/sql-specific)
  (custom/magit-specific)

  (custom/org-specific)
  (custom/markdown-specific)
  (custom/treemacs-specific)
  (custom/helm-specific)

  (custom/zoning)

  (custom/faces)
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
 '(package-selected-packages
   '(org-fancy-priorities shut-up epl git commander f dash s org-wild-notifier ecukes el-mock ert-runner ert-async cask solaire-mode pretty-mode reverse-im nord-theme srcery-theme helpful toml-mode racer flycheck-rust cargo rust-mode org-sticky-header 2048-game dap-mode buffer-expose helm-gtags ggtags erlang counsel-gtags treemacs-evil lsp-ui doom-modeline lsp-mode counsel helm pythonic all-the-icons treemacs zenburn-theme zen-and-art-theme yasnippet-snippets yapfify yaml-mode xterm-color ws-butler writeroom-mode winum white-sand-theme which-key web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package unfill underwater-theme ujelly-theme twilight-theme twilight-bright-theme twilight-anti-bright-theme treemacs-projectile toxi-theme toc-org tao-theme tangotango-theme tango-plus-theme tango-2-theme tagedit symon swiper sunny-day-theme sublime-themes subatomic256-theme subatomic-theme string-inflection stickyfunc-enhance srefactor sql-indent spaceline-all-the-icons spacegray-theme soothe-theme solarized-theme soft-stone-theme soft-morning-theme soft-charcoal-theme snazzy-theme smyx-theme smeargle slim-mode shrink-path shell-pop seti-theme scss-mode sass-mode reverse-theme restart-emacs rebecca-theme realgud ranger rainbow-delimiters railscasts-theme pyvenv pytest pyenv-mode py-isort purple-haze-theme pug-mode professional-theme prodigy prettier-js popwin pony-mode planet-theme pippel pipenv pip-requirements phoenix-dark-pink-theme phoenix-dark-mono-theme pfuture persp-mode pdf-tools pcre2el password-generator paradox overseer orgit organic-green-theme org-projectile org-present org-pomodoro org-mime org-download org-bullets org-brain open-junk-file omtose-phellack-theme oldlace-theme occidental-theme obsidian-theme ob-elixir noctilux-theme naquadah-theme nameless mwim mustang-theme multi-term move-text monokai-theme monochrome-theme molokai-theme moe-theme mmm-mode minimal-theme material-theme markdown-toc majapahit-theme magit-svn magit-gitflow madhat2r-theme macrostep lush-theme lorem-ipsum livid-mode live-py-mode link-hint light-soap-theme kaolin-themes json-navigator js2-refactor js-doc jbeans-theme jazz-theme ir-black-theme inkpot-theme indent-guide importmagic impatient-mode ibuffer-projectile hungry-delete ht hl-todo highlight-parentheses highlight-numbers highlight-indentation heroku-theme hemisu-theme helm-xref helm-themes helm-swoop helm-pydoc helm-purpose helm-projectile helm-org-rifle helm-mode-manager helm-make helm-gitignore helm-git-grep helm-flx helm-descbinds helm-css-scss helm-company helm-c-yasnippet helm-ag hc-zenburn-theme gruvbox-theme gruber-darker-theme grandshell-theme gotham-theme google-translate golden-ratio gnuplot gitignore-templates gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe git-gutter-fringe+ gh-md gandalf-theme fuzzy font-lock+ flyspell-correct-helm flycheck-pos-tip flycheck-mix flycheck-credo flx-ido flatui-theme flatland-theme fill-column-indicator farmhouse-theme fancy-battery eziam-theme eyebrowse expand-region exotica-theme evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-org evil-numbers evil-nerd-commenter evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-goggles evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu espresso-theme eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav eldoc-eval editorconfig dumb-jump dracula-theme dotenv-mode doom-themes dockerfile-mode docker django-theme diminish diff-hl define-word darktooth-theme darkokai-theme darkmine-theme darkburn-theme dakrone-theme cython-mode cyberpunk-theme csv-mode counsel-projectile company-web company-tern company-statistics company-lsp company-anaconda column-enforce-mode color-theme-sanityinc-tomorrow color-theme-sanityinc-solarized clues-theme clean-aindent-mode cherry-blossom-theme centered-cursor-mode busybee-theme bubbleberry-theme browse-at-remote birds-of-paradise-plus-theme badwolf-theme auto-yasnippet auto-highlight-symbol auto-dictionary auto-compile apropospriate-theme anti-zenburn-theme ample-zen-theme ample-theme alect-themes alchemist aggressive-indent afternoon-theme ace-window ace-link ace-jump-helm-line ac-ispell)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
)
