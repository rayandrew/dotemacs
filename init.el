;;;;;;;;;;;;;;;;;;;;;;;;
;; Welkommen
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; R/S Config
;; rs@rs.ht
;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;
;; Constp
;;;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Ray Sinurat")
(setq user-mail-address "rs@rs.ht")

(defconst rs/emacs-dir user-emacs-directory)
(defconst rs/local-dir (concat rs/emacs-dir ".local/"))
(defconst rs/env-file (concat rs/local-dir "env"))
(defconst rs/help-key "C-/")

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
(menu-bar-mode -1) ;; Disable the menu bar
(tool-bar-mode -1) ;; Disable the tool bar
(scroll-bar-mode -1) ;; Disable the scroll bars
(setq inhibit-startup-screen t) ;; Disable splash screen
(setq backup-directory-alist '(("." . "~/.config.d/emacs/backup"))
      backup-by-copying t    
      version-control t      
      delete-old-versions t  
      kept-new-versions 20   
      kept-old-versions 5)
(defalias 'yes-or-no-p 'y-or-n-p)

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
(use-package gruber-darker-theme
 :config
 (load-theme 'gruber-darker t))
;; (use-package gruvbox-theme
;;  :config
;;  (load-theme 'gruvbox-dark-medium t))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertico
;; Completion Framework
;;;;;;;;;;;;;;;;;;;;;;;;
(straight-use-package '(vertico :files (:defaults "extensions/*")
                                :includes (vertico-buffer
                                           vertico-directory
                                           vertico-flat
                                           vertico-indexed
                                           vertico-mouse
                                           vertico-quick
                                           vertico-repeat
                                           vertico-reverse)))

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

;; (use-package vertico-flat
;;   :after vertico
;;   :ensure nil
;;   :init
;;   (vertico-flat-mode t))

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
  :bind (("C-x b" . consult-buffer)))

;; Affe
(use-package affe
  :bind (("C-x p" . affe-find)
	 ("C-x C-p" . affe-grep))
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
	 ("C-/ b" . embark-bindings))
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
;; https://www.reddit.com/r/emacs/comments/f8xwau/hack_replace_execpathfromshell/
;; Credits to DOOM Emacs

(defun rs/update-env ()
  (interactive)
  (shell-command  (concat "printenv > " rs/env-file)))

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
;; (use-package dired-x
;;   :ensure nil
;;   :straight nil)
(use-package dired+)
(define-key dired-mode-map (kbd "-") 'dired-up-directory)
(setq dired-omit-files
      (concat dired-omit-files "\\|^\\..+$"))
(setq-default dired-dwim-target t)
(setq dired-listing-switches "-alh")


;; https://emacs.stackexchange.com/a/36851
(defun rs/dired-copy-path-at-point ()
    (interactive)
    (dired-copy-filename-as-kill 0))
(define-key dired-mode-map (kbd "W") 'rs/dired-copy-path-at-point)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package vterm
  :ensure t)

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
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp SSH password
(setq password-cache-expiry nil)

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

;;;;;;;;;;;;;;;;;;;;;;;;
;; Tramp
;;;;;;;;;;;;;;;;;;;;;;;
;; Fixing tramp cannot get all the path defined in `profile` config
;; https://stackoverflow.com/a/61169654
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
