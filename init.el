;;;;;;;;;;;;;;;;;;;;;;;;
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

;; Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq help-char nil)
(global-set-key (kbd rs/help-key) 'help-command)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Compilation mode
;; (setq compilation-scroll-output t) ;; enable this if you want to follow scrolling
(setq compilation-scroll-output 'first-error)

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

;; Recent Files
(recentf-mode t)
(setq recentf-max-saved-items 50)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Use package
;;;;;;;;;;;;;;;;;;;;;;;;

(straight-use-package 'use-package)
(use-package straight
  :custom
  (straight-use-package-by-default t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme
;;;;;;;;;;;;;;;;;;;;;;;;

(set-face-attribute 'default nil :family "UbuntuMono Nerd Font Mono" :height 160)

;; (use-package gruber-darker-theme
;;   :config
;;   (load-theme 'gruber-darker t))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t    
        doom-themes-enable-italic t)
  (load-theme 'doom-sourcerer t)
  (doom-themes-visual-bell-config)
  (doom-themes-neotree-config)
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-org-config)	       
  (doom-themes-treemacs-config))

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
  :init
  ;; Enable completion by narrowing
  (vertico-mode t)
  ;; Improve directory navigation
  (with-eval-after-load 'vertico
    (define-key vertico-map (kbd "RET") #'vertico-directory-enter)
    (define-key vertico-map (kbd "DEL") #'vertico-directory-delete-word)
    (define-key vertico-map (kbd "M-d") #'vertico-directory-delete-char)))

(use-package vertico-indexed
  :after vertico
  :ensure nil)

(use-package vertico-grid
  :after vertico
  :ensure nil)

(use-package vertico-flat
  :after vertico
  :ensure nil)
  ;; :init
  ;; (vertico-flat-mode t))

(use-package vertico-multiform
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
  :demand t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package savehist
  :init
  (savehist-mode))

(use-package emacs
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
	 ("C-s" . consult-line)))

;; Affe
(use-package affe
  :bind (("C-x C-p" . affe-find)
	 ("C-x C-/" . affe-grep))
  :config
  (consult-customize affe-grep :preview-key "M-."))

;; Marginalia
(use-package marginalia
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :ensure t
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
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package undo-fu-session
  :init
  (undo-fu-session-global-mode))

(use-package vundo
  :ensure t
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
  :bind (("C-x o" . ace-window)))

(use-package windmove
  :bind (("C-l" . windmove-right)
	 ("C-h" . windmove-left)
	 ("C-j" . windmove-down)
	 ("C-k" . windmove-up))
  :config
  (setq windmove-wrap-around nil))

;; (use-package hydra
;;   :demand t
;;   :bind (("C-x" . hydra-other-window/body))
;;   :init
;;   (defhydra hydra-other-window
;;     (global-map "C-x"
;; 		:color red)
;;     "other window"
;;     ("<right>" other-window "→")
;;     ("<left>" (lambda () (interactive) (other-window -1)) "←")))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Dired
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dired
  :ensure nil
  :straight (:type built-in)
  :custom
  (dired-dwim-target t)
  (dired-listing-switches "-alh"))


;; https://emacs.stackexchange.com/a/36851
(defun rs/dired-copy-path-at-point ()
  (interactive)
  (dired-copy-filename-as-kill 0))

(define-key dired-mode-map (kbd "W") 'rs/dired-copy-path-at-point)
(define-key dired-mode-map (kbd "-") 'dired-up-directory)

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
  :config
  (add-hook 'dired-mode-hook 'diredfl-mode))

(use-package dired+
  :after dired
  :init
  (setq diredp-hide-details-initially-flag nil))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vterm
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp
;;;;;;;;;;;;;;;;;;;;;;;;

;; Fixing tramp cannot get all the path defined in `profile` config
;; https://stackoverflow.com/a/61169654
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)

;; Tramp SSH password
(setq password-cache-expiry nil)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Sitter
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tree-sitter)
(use-package tree-sitter-langs
  :after tree-sitter)

(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

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

(defun rs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((lsp-mode . rs/lsp-mode-setup))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :config
  (setq read-process-output-max (* 1024 1024))
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-doc-enable nil))

;;;;;;;;;;;;;;;;;;;;;;;;
;; AutoComplete
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package company
  ;; :after lsp-mode
  :hook (lsp-mode . company-mode)
  :ensure
  :bind
  (:map company-active-map
              ("C-n". company-select-next)
              ("C-p". company-select-previous)
              ("M-<". company-select-first)
              ("M->". company-select-last))
  (:map company-mode-map
        ("<tab>". tab-indent-or-complete)
        ("TAB". tab-indent-or-complete))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

;; Consult LSP
(use-package consult-lsp)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Language
;;;;;;;;;;;;;;;;;;;;;;;;

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
  :demand t
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

;;;;;;;;;;;;;;;;;;;;;;;;
;; Org
;;;;;;;;;;;;;;;;;;;;;;;;

(straight-use-package '(org :files (:defaults "lisp/*")
                                :excludes ()))

(use-package org
  :ensure org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb)
         ("C-c C-w" . org-refile)
         ("C-c j" . org-clock-goto)
         ("C-c C-x C-o" . org-clock-out))
  :config
  (progn
    ;; The GTD part of this config is heavily inspired by
    ;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
    (setq org-directory "~/Notes")
    (setq org-agenda-files
          (mapcar (lambda (path) (concat org-directory path))
                  '("/inbox.org"
                    "/gtd/gtd.org"
                    "/gtd/inbox.org"
                    "/gtd/tickler.org")))

    (setq org-log-done 'time)
    (setq org-src-fontify-natively t)
    (setq org-use-speed-commands t)
    (setq org-capture-templates
          '(("t" "Todo [inbox]" entry
             (file+headline "~/org/gtd/inbox.org" "Tasks")
             "* TODO %i%?")
            ("T" "Tickler" entry
             (file+headline "~/org/gtd/tickler.org" "Tickler")
             "* %i%? \n %^t")))
    (setq org-refile-targets
          '(("~/org/gtd/gtd.org" :maxlevel . 3)
            ("~/org/gtd/someday.org" :level . 1)
            ("~/org/gtd/tickler.org" :maxlevel . 2)))
    (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
    (setq org-agenda-custom-commands
          '(("@" "Contexts"
             ((tags-todo "@email"
                         ((org-agenda-overriding-header "Emails")))
              (tags-todo "@phone"
                         ((org-agenda-overriding-header "Phone")))))))
    (setq org-clock-persist t)
    (org-clock-persistence-insinuate)
    (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))))

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
  
;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;

;; Copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook (prog-mode . copilot-mode)
  ;; :bind (:map copilot-compilation-map
  ;; 	      ("<tab>" . copilot-accept-completion)
  ;; 	      ("TAB" . copilot-accept-completion))
  :config
  (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
  (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion))

;; flymake
(use-package flycheck
  :demand t)

;; Move text
(use-package move-text
  :bind (("M-p" . move-text-up)
	 ("M-n" . move-text-down)))

;; Compilation Mode
;; https://github.com/rexim/dotfiles/blob/25f8ddc6717e56f110e812fd0cec2f1a1dc9d8be/.emacs#L10
compilation-error-regexp-alist-alist
(add-to-list 'compilation-error-regexp-alist
             '("\\([a-zA-Z0-9\\.]+\\)(\\([0-9]+\\)\\(,\\([0-9]+\\)\\)?) \\(Warning:\\)?"
               1 2 (4) (5)))

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
  :ensure nil
  :config
  (global-hl-line-mode))

;; Multiple Cursors
(use-package multiple-cursors
  :defer t)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C-M-SPC")     'set-rectangular-region-anchor)
(global-set-key (kbd "C->")         'mc/mark-next-like-this)
(global-set-key (kbd "C-<")         'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")     'mc/mark-all-like-this)
(global-set-key (kbd "C-\"")        'mc/skip-to-next-like-this)
(global-set-key (kbd "C-:")         'mc/skip-to-previous-like-this)

;; Which Key
(use-package which-key
  :config
  (which-key-mode)
  (lsp-enable-which-key-integration t))

;; Trailing Whitespace
(use-package ws-butler
  :hook (prog-mode-hook . ws-butler-mode))

;; Smartparents
(use-package smartparens
  :diminish smartparens-mode
  :config
  (progn
    (require 'smartparens-config)
    (smartparens-global-mode 1)
    (show-paren-mode t)))

;; Crux
(use-package crux
  :bind
  ("C-k" . crux-smart-kill-line)
  ("C-c n" . crux-cleanup-buffer-or-region)
  ("C-c f" . crux-recentf-find-file)
  ("C-a" . crux-move-beginning-of-line))
