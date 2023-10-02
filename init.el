;; Welkommen
;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; R/S Config
;; rs@rs.ht
;;
;;;;;;;;;;;;;;;;;;;;;;;;

(when (version< emacs-version "27")
  (load (concat user-emacs-directory "early-init.el")))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;

(setq user-full-name "Ray Sinurat")
(setq user-mail-address "rs@rs.ht")

(defconst rs/emacs-dir user-emacs-directory)
(defconst rs/local-dir (concat rs/emacs-dir ".local/"))
(defconst rs/env-file (concat rs/local-dir "env"))
(defconst rs/help-key "C-?")
(defconst rs/column-indicator 100)
(defconst rs/org-dir "~/Brain")

;;;;;;;;;;;;;;;;;;;;;;;;
;; Straight
;; Package Manager
;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	    (url-retrieve-synchronously
	     "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	     'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Default
;;;;;;;;;;;;;;;;;;;;;;;;

(setq initial-major-mode 'fundamental-mode)
(setq read-process-output-max (* 1024 1024))
(setq backup-directory-alist '(("." . "~/.config.d/emacs/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; Silent Warning Comp
;; https://www.reddit.com/r/emacs/comments/11a4jz4/emacs_automatically_switches_to_warnings_how_to/
(setq native-comp-async-report-warnings-errors 'silent)

;; Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq help-char nil)
(global-set-key (kbd rs/help-key) 'help-command)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Shell mode
;; (setq shell-file-name "zsh")
;; (setq shell-command-switch "-ic")

;; Splitting behavior
(setq split-width-threshold nil)

(defun rs/reload-init ()
  (interactive)
  (load-file user-init-file))
(define-key global-map (kbd "C-M-r") 'rs/reload-init)

;; (electric-pair-mode 1)
;; (electric-indent-mode -1)

;; Recent Files
(recentf-mode t)
(setq recentf-max-saved-items 50)

;; Column Indicator
(setq-default display-fill-column-indicator-column rs/column-indicator)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; Restore last line of edited files
(save-place-mode 1)

;; Delete without copy
;; https://stackoverflow.com/a/71668527
(defun rs/delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))
(defun rs/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (rs/delete-word (- arg)))

(global-set-key (kbd "M-<backspace>") 'rs/backward-delete-word)
(global-set-key (kbd "C-<backspace>") 'rs/backward-delete-word)
(global-set-key (kbd "C-<delete>") 'rs/delete-word)
(global-set-key (kbd "M-d") 'rs/delete-word)

;; Quit with confirmation
(setq confirm-kill-emacs 'y-or-n-p) 

;;;;;;;;;;;;;;;;;;;;;;;;
;; Use package
;;;;;;;;;;;;;;;;;;;;;;;;

(straight-use-package 'use-package)
(use-package straight
  :custom
  (straight-use-package-by-default t))
;; (setq use-package-verbose t)      

;;;;;;;;;;;;;;;;;;;;;;;;
;; Auto Update Package
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package auto-package-update
  :custom
  (auto-package-update-interval 7)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package gruber-darker-theme
  :config
  (load-theme 'gruber-darker t))

;; (use-package badger-theme
;;   :config (load-theme 'badger t))

(use-package all-the-icons)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic
;;;;;;;;;;;;;;;;;;;;;;;;

;; Scrolling
;; https://pragmaticemacs.wordpress.com/2015/07/14/scrolling-and-moving-by-line/
(setq scroll-preserve-screen-position 1)
;;scroll window up/down by one line
(global-set-key (kbd "M-<up>") (kbd "C-u 1 M-v"))
(global-set-key (kbd "M-<down>") (kbd "C-u 1 C-v"))


;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation Mode
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package compile
  :commands (compile)
  :straight (:type built-in)
  :bind (("C-x c" . compile))
  :config
  ;; Compilation Mode
  ;; (setq compilation-scroll-output t) ;; enable this if you want to follow scrolling
  (setq compilation-scroll-output 'first-error)
  ;; https://github.com/rexim/dotfiles/blob/25f8ddc6717e56f110e812fd0cec2f1a1dc9d8be/.emacs#L10
  compilation-error-regexp-alist-alist
  (add-to-list 'compilation-error-regexp-alist
	           '("\\([a-zA-Z0-9\\.]+\\)(\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?) \\(Warning:\\)?"
	             1 2 (4) (5))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package mood-line
  :config
  (mood-line-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertico
;; Completion Framework
;;;;;;;;;;;;;;;;;;;;;;;;

(straight-use-package '(vertico :files (:defaults "extensions/*")
				                :includes (vertico-buffer
					                       vertico-directory
					                       vertico-flat
					                       vertico-grid
					                       vertico-indexed
					                       vertico-mouse
					                       vertico-quick
					                       vertico-repeat
					                       vertico-reverse
					                       vertico-multiform)))

(use-package vertico
  :ensure t
  :defer t
  :init
  (vertico-mode t))

(use-package vertico-indexed
  :defer t
  :after vertico
  :ensure nil)

(use-package vertico-grid
  :defer t
  :after vertico
  :ensure nil)

(use-package vertico-flat
  :defer t
  :after vertico
  :ensure nil)

(use-package vertico-multiform
  :defer 0
  :after vertico
  :ensure nil
  :init
  (vertico-multiform-mode)
  :config
  (setq vertico-multiform-categories
	    '((file flat (vertico-cycle . t))))
  (setq vertico-multiform-commands
	    '((consult-imenu buffer indexed)
	      (affe-find buffer indexed)
	      (affe-grep buffer indexed)
	      (execute-extended-command flat (vertico-cycle . t)))))

(use-package vertico-repeat
  :after vertico
  :ensure nil
  :bind (("M-r" . vertico-repeat))
  :hook (minibuffer-setup . vertico-repeat-save))

(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
	          ("RET" . vertico-directory-enter)
	          ("DEL" . vertico-directory-delete-char)
	          ("M-DEL" . vertico-directory-delete-word))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :defer t
  :init
  (setq completion-styles '(orderless partial-completion basic)
	    completion-category-defaults nil
	    completion-category-overrides '((file (styles basic partial-completion)))))

(use-package savehist
  :defer 1
  :init
  (savehist-mode))

(use-package emacs
  :defer 1
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
		          (replace-regexp-in-string
		           "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
		           crm-separator)
		          (car args))
	      (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  (setq minibuffer-prompt-properties
	    '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  (setq enable-recursive-minibuffers t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Consult
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package consult
  :bind (("C-x b" . consult-buffer)
	     ("C-x r" . consult-recent-file)
	     ("C-:" . consult-goto-line)
	     ("C-x /" . consult-line)
	     ("C-x C-/" . consult-grep)))

(global-set-key (kbd "C-x C-p") 'project-find-file)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Marginalia
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package marginalia
  :bind (:map minibuffer-local-map
	          ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Embark
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package embark
  :bind (("C-." . embark-act)
	     ("C-;" . embark-dwim)
	     ("C-<return>" . embark-bindings))
  :init
  (setq embark-help-key rs/help-key)
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  :config
  (add-to-list 'display-buffer-alist
	           '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		         nil
		         (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package undo-fu-session
  :defer 0
  :init
  (undo-fu-session-global-mode))

(use-package vundo
  :commands (vundo vundo-backward vundo-forward)
  :bind (("C-M-u" . vundo)
	     :map vundo-mode-map
         ("l" . vundo-forward)
	     ("h" . vundo-backward)
	     ("j" . vundo-next)
	     ("k" . vundo-previous)
	     ("<right>" . vundo-forward)
	     ("<left>" . vundo-backward)
	     ("<up>" . vundo-next)
	     ("<down>" . vundo-previous)
	     ("C-a" . vundo-stem-root)
	     ("C-e" . vundo-stem-end)
	     ("RET" . vundo-confirm)
	     ("q" . vundo-quit)
	     ("C-g" . vundo-quit))
  :config
  (setq vundo-compact-display t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; ENV
;;;;;;;;;;;;;;;;;;;;;;;;

(defun rs/update-env ()
  (interactive)
  (shell-command  (concat "printenv > " rs/env-file)))

;; https://www.reddit.com/r/emacs/comments/f8xwau/hack_replace_execpathfromshell/
;; Credits to DOOM Emacs
(defun rs/load-envvars-file (file &optional noerror)
  "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
  (if (not (file-readable-p file))
      (unless noerror
	    (signal 'file-error (list "Couldn't read envvar file" file)))
    (let (envvars environment)
      (with-temp-buffer
	    (save-excursion
	      (insert "\n")
	      (insert-file-contents file))
	    (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
	      (push (match-string 1) envvars)
	      (push (buffer-substring
		         (match-beginning 1)
		         (1- (or (save-excursion
			               (when (re-search-forward "^\\([^= ]+\\)=" nil t)
			                 (line-beginning-position)))
			             (point-max))))
		        environment)))
      (when environment
	    (setq process-environment
	          (append (nreverse environment) process-environment)
	          exec-path
	          (if (member "PATH" envvars)
		          (append (split-string (getenv "PATH") path-separator t)
			              (list exec-directory))
		        exec-path)
	          shell-file-name
	          (if (member "SHELL" envvars)
		          (or (getenv "SHELL") shell-file-name)
		        shell-file-name))
	    envvars))))

(when (and (or (display-graphic-p)
	           (daemonp))
	       (file-exists-p rs/env-file))
  (rs/load-envvars-file rs/env-file))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Windowing
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
  :bind (("C-x C-o" . ace-window)))

(use-package windmove
  :straight (:type built-in)
  :bind (("C-l" . windmove-right)
	     ("C-h" . windmove-left)
	     ("C-j" . windmove-down)
	     ("C-k" . windmove-up))
  :config
  (setq windmove-wrap-around nil))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;

;; https://emacs.stackexchange.com/a/36851
(defun rs/dired-copy-path-at-point ()
  (interactive)
  (dired-copy-filename-as-kill 0))

(use-package dired
  :commands (dired)
  :straight (:type built-in)
  :bind (("C-x e" . dired-jump)
         (:map dired-mode-map
               ("-" . dired-up-directory)
               ("W" . rs/dired-copy-path-at-point)))
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-Alh"))
  ;; (dired-listing-switches "-Alht --group-directories-first"))

(use-package dired-x
  :after dired
  :ensure nil
  :straight nil
  :config
  (setq dired-omit-files
	    (concat dired-omit-files "\\|^\\..+$")))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
	          ("i" .  dired-subtree-toggle)))

(use-package diredfl
  :after dired
  :hook (dired-mode . diredfl-mode))

(use-package dired+
  :after dired
  :init
  (setq diredp-hide-details-initially-flag nil))

(use-package dired-single
  :after dired)

;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-open
  :after dired
  :bind (:map dired-mode-map
              ("C-o" . dired-open-xdg)))
  ;; :config
  ;; Doesn't work as expected!
  ;; (add-to-list 'dired-open-functions #'dired-open-xdg t)
  ;; (setq dired-open-extensions '(("mp4" . "feh")
  ;;                               ("mkv" . "mpv"))))

  (use-package dired-hide-dotfiles
    :hook (dired-mode . dired-hide-dotfiles-mode)
    :bind (:map dired-mode-map
                ("H" . dired-hide-dotfiles-mode)))

;; (define-key dired-mode-map (kbd "W") 'rs/dired-copy-path-at-point)
;; (define-key dired-mode-map (kbd "-") 'dired-up-directory)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")  ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh")                       ;; Set this to customize the shell to launch
  (setq vterm-max-scrollback 10000))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp
;;;;;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load 'tramp
  ;; Fixing tramp cannot get all the path defined in `profile` config
  ;; https://stackoverflow.com/a/61169654
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (setq-default tramp-shell-prompt-pattern
	            "\\(?:^\\|\r\\)[^]#$%>\n]*#?[]#$%>].* *\\(^[\\[[0-9;]*[a-zA-Z] *\\)*")

  ;; Tramp SSH password
  (setq password-cache-expiry nil))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :bind (("C-x g" . magit)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Lsp
;;;;;;;;;;;;;;;;;;;;;;;;

(straight-use-package '(lsp-mode :files (:defaults "clients/*")
				                 :includes (lsp-nix)))

(use-package lsp-mode
  :after (corfu cape)
  :commands (lsp lsp-deferred)
  :custom
  (lsp-completion-provider :none) ;; using corfu
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  (lsp-inlay-hint-enable nil) ;; disable inlay hint
  ;; Rust
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :init
  (setq lsp-keymap-prefix "C-c l")

  (defun rs/orderless-dispatch-flex-first (_pattern index _total)
    (and (eq index 0) 'orderless-flex))

  (defun rs/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  ;; Optionally configure the first word as flex filtered.
  (add-hook 'orderless-style-dispatchers #'rs/orderless-dispatch-flex-first nil 'local)

  ;; Optionally configure the cape-capf-buster.
  (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))

  ;; (defun rs/lsp-mode-setup-completion ()
  ;;   (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
  ;;         '(flex))) ;; Configure flex  
  ;; (defun rs/lsp-mode-setup ()
  ;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  ;;   (lsp-headerline-breadcrumb-mode))
  
  :hook ((lsp-mode . rs/lsp-mode-setup)
	     (lsp-mode . lsp-enable-which-key-integration)
	     (lsp-mode . lsp-ui-mode)  
	     (lsp-completion-mode . rs/lsp-mode-setup-completion))
  :config
  (setq read-process-output-max (* 1024 1024)))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-enable nil) ;; disable sideline hint
  (lsp-ui-doc-enable nil))

;;;;;;;;;;;;;;;;;;;;;;;;
;; AutoComplete
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package corfu
  :defer 1
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  :init
  (global-corfu-mode)
  :config
  ;; TAB cycle if there are only few candidates
  (setq completion-cycle-threshold 3)
  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (setq tab-always-indent 'complete))

(use-package yasnippet
  :after corfu
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package cape
  :after corfu)

(use-package dabbrev
  ;; Swap M-/ and C-M-/
  :bind (("M-/" . dabbrev-completion)
         ("C-M-/" . dabbrev-expand))
  ;; Other useful Dabbrev configurations.
  :custom
  (dabbrev-ignored-buffer-regexps '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Language
;;;;;;;;;;;;;;;;;;;;;;;;

;; C
(setq-default c-basic-offset 4)
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

;; Latex
(use-package lsp-latex
  :ensure lsp-mode
  :after lsp-mode
  :demand t
  :hook ((latex-mode-hook . lsp-deferred)
	     (tex-mode-hook . lsp-deferred)
	     (bibtex-mode-hook . lsp-deferred)
	     (yatex-mode-hook . lsp-deferred)))

;; Nix
(use-package lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :custom
  (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)

;; Rust
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
	          ("M-j" . lsp-ui-imenu)
	          ("M-?" . lsp-find-references)
	          ("C-c C-c l" . flycheck-list-errors)
	          ("C-c C-c a" . lsp-execute-code-action)
	          ("C-c C-c r" . lsp-rename)
	          ("C-c C-c q" . lsp-workspace-restart)
	          ("C-c C-c Q" . lsp-workspace-shutdown)
	          ("C-c C-c s" . lsp-rust-analyzer-status)
	          ("C-c C-c e" . lsp-rust-analyzer-expand-macro)
	          ("C-c C-c d" . dap-hydra)
	          ("C-c C-c h" . lsp-ui-doc-glance))
  :config
  ;; less flashiness
  (setq lsp-eldoc-hook nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)

  ;;  rustfmt on save
  (add-hook 'rustic-mode-hook 'rs/rustic-mode-hook))

(defun rs/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))

;; Python
(use-package python-mode
  :straight (:type built-in)
  :mode ("\\.py\\'" . python-mode))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "<backspace>") 'delete-backward-char)
            (define-key python-mode-map (kbd "<S-tab>") 'python-indent-dedent-line)))

;; Powershell
(use-package powershell
  :commands (powershell powershell-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; RS Mode
;;;;;;;;;;;;;;;;;;;;;;;;

;; https://emacs.stackexchange.com/questions/47276/emacs-custom-major-mode-with-multiple-comment-types

(defvar rs-mode-hook nil)

(defvar rs-mode-map
  (let ((map (make-keymap)))
    map))

(defvar rs-mode-syntax-table
  (with-syntax-table (copy-syntax-table)
    ;; comment
    (modify-syntax-entry ?\; ". 12b")
    ;; (modify-syntax-entry ?! ".")
    (modify-syntax-entry ?\n "> b")

    ;; (modify-syntax-entry ?\/ ". 14")
    ;; (modify-syntax-entry ?* ". 23")

    (modify-syntax-entry ?\' ".")
    (modify-syntax-entry ?\" "\"")

    (modify-syntax-entry ?\[ "(^")
    (modify-syntax-entry ?\] ")$")
    (syntax-table))
  "'rs-mode' syntax table ")

(defgroup rs nil
  "Major mode for editing RS code."
  :prefix 'rs
  :group 'languages)

(defvar rs-font-lock-keywords
  (list '("[\[[A-Za-z0-9_-]+\]" . font-lock-warning-face)
        '("^===\s+[A-Za-z0-9_-,]+.*===" . font-lock-constant-face)
	    '("--\\(\[A-Za-z0-9_-]+\\)" . font-lock-type-face)
	    '("!!.+" . font-lock-warning-face)
	    '("#.+" . font-lock-string-face)
	    ;; '(">.+" . font-lock-variable-name-face)
	    '("^\s*-.+" . font-lock-function-name-face)
	    ;; '("--\\(\[A-Za-z0-9_-]+\\)" . font-lock-type-face)
	    '("^\s*>.+" . font-lock-keyword-face)
	    ;; '("^\s*>\s[A-Za-z0-9_-].*" 0 font-lock-keyword-face)
	    ;; '("^\s*>\s[A-Za-z0-9_-].*" 1 font-lock-keyword-face)
	    ;; '("^\s*$\s[A-Za-z0-9_-].*" 1 font-lock-constant-face)
	    ;; '("#.*[A-Za-z0-9-_].*")
	    ;; '("\\(\$[0-9]+\\)[^0-9]" 1 font-lock-constant-face)
	    '("\$[A-Za-z0-9_\-]+" . font-lock-variable-name-face)))

(define-derived-mode rs-mode prog-mode "RS"
  :syntax-table rs-mode-syntax-table
  :group 'rs
  (setq-local font-lock-defaults '(rs-font-lock-keywords))
  (setq-local comment-start ";;")
  (font-lock-fontify-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;

(straight-use-package '(org :files (:defaults "lisp/*")
			                :excludes ()))

(message (expand-file-name "gtd/inbox.org" rs/org-dir))

(use-package org
  :ensure org-plus-contrib
  :commands (org-insert-link-global org-open-at-point-global)
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
	     ("C-c c" . org-capture)
	     ("C-c a" . org-agenda)
	     ("C-c b" . org-iswitchb)
	     ("C-c C-w" . org-refile)
	     ("C-c j" . org-clock-goto)
         ("C-c C-o" . org-open-at-point-global)
         ("C-c C-l" . org-insert-link-global))
  :config
  (progn
    ;; The GTD part of this config is heavily inspired by
    ;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
    (setq org-directory rs/org-dir)
    (setq org-agenda-files
	      (mapcar (lambda (path) (expand-file-name path org-directory))
		          '("/inbox.org"
		            "/gtd/gtd.org"
		            "/gtd/inbox.org"
		            "/gtd/tickler.org")))

    (setq org-log-done 'time)
    (setq org-src-fontify-natively t)
    (setq org-use-speed-commands t)
    (setq org-capture-templates
	      `(("t" "Todo [inbox]" entry (file+headline ,(expand-file-name "gtd/inbox.org" rs/org-dir) "Tasks") "* TODO %i%?")
	        ("T" "Tickler" entry (file+headline ,(expand-file-name "gtd/tickler.org" rs/org-dir) "Tickler") "* %i%? \n %^t")))
    (setq org-refile-targets
	      '(((expand-file-name "gtd/inbox.org" rs/org-dir) :maxlevel . 3)
	        ((expand-file-name "gtd/someday.org" rs/org-dir) :level . 1)
	        ((expand-file-name "gtd/tickler.org" rs/org-dir) :maxlevel . 2)))
    (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
    (setq org-agenda-custom-commands
	      '(("@" "Contexts"
	         ((tags-todo "@email"
			             ((org-agenda-overriding-header "Emails")))
	          (tags-todo "@phone"
			             ((org-agenda-overriding-header "Phone")))))))
    (setq org-clock-persist t)
    (org-clock-persistence-insinuate)
    (setq org-time-clocksum-format '(:hours "%d"
				                            :require-hours t
				                            :minutes ":%02d"
				                            :require-minutes t))))

(defun rs/org-capture-inbox ()
  (interactive)
  (org-capture nil "i"))

(defun rs/org-capture-slipbox ()
  (interactive)
  (org-capture nil "s"))

(defun rs/org-agenda ()
  (interactive)
  (org-agenda nil " "))

(global-set-key (kbd "C-c <tab>") #'rs/org-capture-inbox)
(global-set-key (kbd "C-c SPC") #'rs/org-agenda)

;; (global-set-key (kbd "C-c C-o") 'org-open-at-point-global)
;; (global-set-key (kbd "C-c C-l") 'org-insert-link-global)

(use-package org-inlinetask
  :ensure nil
  :straight (:type built-in)
  :after org
  :bind (:map org-mode-map
	          ("C-c C-x t" . org-inlinetask-insert-task))
  :after (org)
  :commands (org-inlinetask-insert-task))

(use-package org-bullets
  :after org
  :commands (org-bullets-mode)
  :hook (org-mode . org-bullets-mode)
  ;; :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :custom
  ( org-bullets-bullet-list
    '(;;; Large
      ;; "◉"
      ;; "○"
      ;; "✸"
      ;; "✿"
      ;; ♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ☢ ❀ ◆ ◖ ▶
    ;;; Small
      "►"
      "•"
      "★"
      "▸"
      )))

;; (use-package linkmarks
;;   :straight (linkmarks :type git :host github :repo "dustinlacewell/linkmarks")
;;   :bind (("C-c b" . linkmarks-select)
;;          ;; ("C-c b" . linkmarks-capture)
;;          )
;;   :init
;;   (setq linkmarks-file "~/Brain/bookmarks.org")
;;   ;; (setq linkmarks-file (expand-file-name "bookmarks.org" rs/org-dir))
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;

;; Commenter
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; Paredit
(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
	     (clojure-mode . paredit-mode)
	     (lisp-mode . paredit-mode)
	     (common-lisp-mode . paredit-mode)
	     (scheme-mode . paredit-mode)
	     (racket-mode . paredit-mode)))

;; Copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :commands (copilot-mode)
  ;; :bind (:map copilot-compilation-map
  ;;               ("<tab>" . copilot-accept-completion)
  ;;               ("TAB" . copilot-accept-completion))
  :config
  (define-key copilot-completion-map (kbd "M-<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "M-TAB") 'copilot-accept-completion))

;; flymake
(use-package flycheck
  :defer t)

;; Move text
(use-package move-text
  :bind (("M-p" . move-text-up)
	     ("M-n" . move-text-down)))

;; Zoom In/Out
;; https://stackoverflow.com/a/60641769
;; Resize the whole frame, and not only a window
;; Adapted from https://stackoverflow.com/a/24714383/5103881

(defun rs/zoom-frame (&optional amt frame)
  "Increaze FRAME font size by amount AMT. Defaults to selected
frame if FRAME is nil, and to 1 if AMT is nil."
  (interactive "p")
  (let* ((frame (or frame (selected-frame)))
	     (font (face-attribute 'default :font frame))
	     (size (font-get font :size))
	     (amt (or amt 1))
	     (new-size (+ size amt)))
    (set-frame-font (font-spec :size new-size) t `(,frame))
    (message "Frame's font new size: %d" new-size)))

(defun rs/zoom-frame-out (&optional amt frame)
  "Call `rs/zoom-frame' with negative argument."
  (interactive "p")
  (rs/zoom-frame (- (or amt 1)) frame))

(global-set-key (kbd "C-=") 'rs/zoom-frame)
(global-set-key (kbd "C--") 'rs/zoom-frame-out)
(global-set-key (kbd "<C-down-mouse-4>") 'rs/zoom-frame)
(global-set-key (kbd "<C-down-mouse-5>") 'rs/zoom-frame-out)

;; Highlight
(use-package hl-line
  :defer 2
  :ensure nil
  :config
  (global-hl-line-mode))

;; Multiple Cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-\"" . mc/skip-to-next-like-this)
         ("C-:" . mc/skip-to-previous-like-this)))

;; (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
;; (global-set-key (kbd "C-M-SPC")     'set-rectangular-region-anchor)
;; (global-set-key (kbd "C->")         'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
;; (global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
;; (global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
;; (global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; Which Key
(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1)) 

;; Trailing Whitespace
(use-package ws-butler
  :hook (pog-mode-hook . ws-butler-mode))

;; Smartparents
(use-package smartparens
  :defer 2
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

;; Crux
(use-package crux
  :bind
  ("C-c C-k" . crux-smart-kill-line)
  ("C-c C-n" . crux-cleanup-buffer-or-region)
  ("C-c C-f" . crux-recentf-find-file)
  ("C-c C-a" . move-beginning-of-line)
  ("C-a" . crux-move-beginning-of-line))

;; Rainbow
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; PDF
(use-package pdf-tools
  :load-path "site-lisp/pdf-tools/lisp"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Email
;;;;;;;;;;;;;;;;;;;;;;;;

;; credits to https://cadadr.dreamwidth.org/828.html
(defun rs/executable-ensure (command &optional silent)
  "Err-out if COMMAND is not found."
  (if-let* ((ex (executable-find command)))
      ex
    (when (not silent)
      (warn "Program is absent: %s" command))))

;; Setup default OPENPGP key
(setq mml-secure-openpgp-sign-with-sender t)

(use-package notmuch
  :commands (notmuch)
  :bind (("C-c m" . notmuch)
         (:map notmuch-show-mode-map
               ("v" . notmuch-show-view-part)))
  :custom
  (notmuch-crypto-process-mime t)
  (notmuch-fcc-dirs '(("rs@rs.ht" . "rs/Sent")
                      ("rayandrew@uchicago.edu" . "uchicago/Sent")
                      ("raydreww@gmail.com" . "\"raydreww/[Gmail]/Sent Mail\"")))
  (notmuch-show-logo nil)
  (notmuch-always-prompt-for-sender 't)
  (message-default-mail-headers "Cc: \nBcc: \n") ;; Always show BCC
  (notmuch-search-oldest-first nil)
  (notmuch-show-indent-content nil)
  (notmuch-saved-searches '((:name "inbox"
                                   :query "tag:unread"
                                   :sort-order newest-first
                                   :key ,(kbd "i"))
                            (:name "ucare"
                                   :query "tag:ucare or tag:haryadi"
                                   :sort-order newest-first
                                   :key ,(kbd "h"))
                            (:name "teaching"
                                   :query "tag:teaching"
                                   :sort-order newest-first
                                   :key ,(kbd "o"))
                            (:name "sent"
                                   :query "tag:sent"
                                   :key ,(kbd "t"))
                            (:name "all mail"
                                   :query "*"
                                   :key ,(kbd "a"))))
  :config
  (turn-on-gnus-dired-mode)
  (add-hook 'notmuch-hello-mode-hook
            (lambda () (display-line-numbers-mode 0)))
  (setq message-send-mail-function 'message-send-mail-with-sendmail
        message-sendmail-f-is-eval t
        ;; message-sendmail-extra-arguments '("--read-envelope-from")
        message-sendmail-envelope-from 'header
        mail-specify-envelope-from 't
        mail-envelope-from 'header
        sendmail-program (rs/executable-ensure "msmtp")))

(use-package ol-notmuch
  :after (notmuch org-mode))

(use-package messages-are-flowing
  :hook (message-mode-hook . messages-are-flowing-use-and-mark-hard-newlines))

(add-hook 'message-mode-hook (lambda ()
                               (electric-indent-local-mode -1)
                               (setq-local truncate-lines nil)))

;; (add-hook 'message-setup-hook 'mml-secure-sign-pgpmime)
(add-hook 'message-setup-hook 'mml-secure-message-sign-pgpmime)

;; https://emacs.stackexchange.com/a/41176
(defun rs/confirm-empty-subject ()
  "Allow user to quit when current message subject is empty."
  (or (message-field-value "Subject")
      (yes-or-no-p "Really send without Subject? ")
      (keyboard-quit)))

(add-hook 'message-send-hook #'rs/confirm-empty-subject)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Browser
;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package eaf
;;   :straight (eaf :type git
;;                  :host github
;;                  :repo "emacs-eaf/emacs-application-framework"
;;                  :files ("*.el" ".py" "core" "app" "*.json")
;;                  :includes (eaf-pdf-viewer eaf-browser) ; Straight won’t try to search for these packages when we make further use-package invocations for them
;;                  :pre-build (("python3" "install-eaf.py"))))

;; (use-package eaf-browser
;;   :custom
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t))

(use-package eww
  :commands eww eww-follow-link
  :init
  ;; (setq browse-url-browser-function 'eww-browse-url)
  (setq eww-search-prefix "http://www.google.com/search?q=")
  (defun eww-wiki (text)
    "Function used to search wikipedia for the given text."
    (interactive (list (read-string "Wiki for: ")))
    (eww (format "https://en.m.wikipedia.org/wiki/Special:Search?search=%s"
                 (url-encode-url text))))
  :bind (("C-c w w" . eww)
         ("C-c w i" . eww-wiki)
         ("C-c w l" . eww-follow-link)))

(use-package ace-link
  :defer 1
  ;; :hook (eww-mode . ace-link-mode)
  :config
  (ace-link-setup-default))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Epilogue
;;;;;;;;;;;;;;;;;;;;;;;;

(put 'downcase-region 'disabled nil)
(global-unset-key (kbd "C-z"))
