;;;;;;;;;;;;;;;;;;;;;;;;
;; Welkommen
;;;;;;;;;;;;;;;;;;;;;;;;
;; R/S Config
;; rs at rs.ht
;;;;;;;;;;;;;;;;;;;;;;;;
;; Copied shamlessly from
;; https://github.com/daviwil/emacs-from-scratch
;; https://bitbucket.org/grumph/home_config/src/master/.emacs.d/init.el

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
(defconst rs/hg-column-indicator 75)
(defconst rs/org-dir "~/Org")
(defconst rs/phd-dir "~/phd")
(defconst rs/lib-dir (concat (expand-file-name "lib" user-emacs-directory) "/"))

(setq calendar-location-name "Chicago, IL")
(setq calendar-latitude 41.8)
(setq calendar-longitude -87.6)

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
(setq backup-directory-alist `(("." . "~/.config.d/emacs/backup"))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name  "auto-save/" rs/emacs-dir) t)))
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

;; Pixel-wise stuff
(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise t)
(when (>= emacs-major-version 26)
  (pixel-scroll-mode))

(setq help-char nil)
(global-set-key (kbd rs/help-key) 'help-command)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; Shell mode
;; (setq shell-file-name "zsh")
;; (setq shell-command-switch "-ic")

;; Splitting behavior
(setq split-width-threshold nil)

;; Reload config
(defun rs/reload-init ()
  (interactive)
  (load-file user-init-file))
(define-key global-map (kbd "C-M-r") 'rs/reload-init)

;; Disable pair
;; (electric-pair-mode 1)

;; Disable electric indent mode (clunky)
;; (electric-indent-mode -1)
;; (add-hook 'after-change-major-mode-hook (lambda() (electric-indent-mode -1)))

;; Recent Files
(recentf-mode t)
(setq recentf-max-saved-items 50)

;; Restore last line of edited files
(save-place-mode 1)

;; Quit with confirmation
(setq confirm-kill-emacs 'y-or-n-p)

; Smooth scroll
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq scroll-margin 2) ;; keep x lines of border
(setq scroll-conservatively 15) ;; if cursor goes out of the screen less than x lines, do not recenter on it, just continue scrolling
(setq hscroll-step 1)
(setq hscroll-margin 15)
(setq scroll-preserve-screen-position t)

;; defer fontification while input pending
(setq jit-lock-defer-time 0.1)

;; Better performance with files with long lines
(global-so-long-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Column Indicator
;;;;;;;;;;;;;;;;;;;;;;;;

(defun rs/default-column-indicator ()
  (interactive)
  (setq-default display-fill-column-indicator-column rs/column-indicator)
  (display-fill-column-indicator-mode))

(defun rs/hg-column-indicator ()
  (interactive)
  (setq-default display-fill-column-indicator-column rs/hg-column-indicator)
  (display-fill-column-indicator-mode))

;; (setq-default display-fill-column-indicator-column rs/column-indicator)
;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

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

;; Discard all themes before loading new.
(define-advice load-theme (:before (&rest _args) theme-dont-propagate)
  "Discard all themes before loading new."
  (mapc #'disable-theme custom-enabled-themes))

;; (use-package gruber-darker-theme
;;   :config
;;   (load-theme 'gruber-darker t))

(use-package naysayer-theme
  :defer t
  :ensure nil
  :straight nil
  :config
  ;; (load-theme 'naysayer t)
  :load-path "~/.config/emacs/lib")

(use-package doom-themes
  :defer t
  :config
  (setq doom-themes-enable-bold t ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (doom-themes-org-config))

(use-package circadian
  :ensure t
  :config
  (setq circadian-themes '(("8:00" . doom-solarized-light)
                           ("15:00" . doom-wilmersdorf)))
  (circadian-setup))

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
;; Overriding keybindings from packages
;;;;;;;;;;;;;;;;;;;;;;;;

;; Better C-k handling
(defun rs/kill-and-join-forward (&optional arg)
  "If at end of line, remove newline and join with the following line.
Otherwise kill current line.  Whitespaces are ignored.
Argument ARG: see \"kill-line\" doc."
  (interactive "P")
  (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
             (just-one-space 0)
             (backward-char 1)
             (kill-line arg))
    (kill-line arg)))

(defun rs/forward-word (&optional arg)
  "Go to closest point: \"end-of-line\", \"forward-word\" or \"forward-to-word\".
Check these functions for ARG value."
  (interactive)
  (setq arg (or arg 1))
  (let ((p-to (save-excursion
                (forward-to-word arg)
                (point)))
        (p (save-excursion
             (forward-word arg)
             (point)))
        (p-end (save-excursion
                 (end-of-line)
                 (point)))
        )
    (if (and (< p-end p) (< p-end p-to) (/= p-end (point)))
        (goto-char p-end) ;; Move to end of line after last word
      (if (or
           (and (< p-to p)  (< (+ (point) 1) p-to))
           (>= (+ (point) 2) p))
          (goto-char p-to)
        (goto-char p)))))

(defun rs/backward-word (&optional arg)
  "Go to closest point following different methods.
Functions are\"back-to-indentation\", \"backward-word\" or \"backward-to-word\".
Check these functions for ARG value."
  (interactive)
  (setq arg (or arg 1))
  (let ((p-to (save-excursion
                (backward-to-word arg)
                (point)))
        (p (save-excursion
             (backward-word arg)
             (point)))
        (p-start (save-excursion
             (back-to-indentation)
             (point)))
        )
    (if (and (> p-start p) (> p-start p-to) (/= p-start (point)))
        (goto-char p-start)             ;; Move to indentation before first word
      (if (or (and (> p-to p)  (> (- (point) 1) p-to))
              (<= (- (point) 2) p))
          (goto-char p-to)
        (goto-char p)))))

(defun rs/forward-kill-word (&optional arg)
  "Kill the next word with \"rs/forward-word\".
Check this function for ARG value."
  (interactive)
  (setq arg (or arg 1))
  (let ((p (save-excursion
            (rs/forward-word arg)
            (point))))
    (delete-region (point) (goto-char p))))

(defun rs/backward-kill-word (&optional arg)
  "Kill the previous word with \"rs/backward-word\".
Check this function for ARG value."
  (interactive)
  (setq arg (or arg 1))
  (let ((p (save-excursion
            (rs/backward-word arg)
            (point))))
    (delete-region (point) (goto-char p))))

;; Delete without copy
;; https://stackoverflow.com/a/71668527
;; (defun rs/delete-word (arg)
;;   "Delete characters forward until encountering the end of a word.
;; With argument ARG, do this that many times."
;;   (interactive "p")
;;   (delete-region (point) (progn (forward-word arg) (point))))

(defun rs/backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (rs/delete-word (- arg)))

;; (global-set-key (kbd "C-<up>")     'backward-paragraph)
;; (global-set-key (kbd "C-<down>")   'forward-paragraph)
;; (global-set-key (kbd "C-<right>")  'rs/forward-word)
;; (global-set-key (kbd "C-<left>")   'rs/backward-word)
;; (global-set-key (kbd "M-<backspace>") 'rs/backward-delete-word)
;; (global-set-key (kbd "C-<backspace>") 'rs/backward-delete-word)
;; (global-set-key (kbd "C-<delete>") 'rs/delete-word)
;; (global-set-key (kbd "M-d") 'rs/delete-word)

(global-set-key [remap forward-word] 'rs/forward-word)
(global-set-key [remap backward-word] 'rs/backward-word)
(global-set-key [remap backward-kill-word] 'rs/backward-kill-word)
(global-set-key [remap kill-word] 'rs/forward-kill-word)

(global-set-key (kbd "C-\\") 'kill-line)
(global-set-key [remap kill-line] 'rs/kill-and-join-forward)

(global-set-key (kbd "C-`") 'recenter-top-bottom)
(global-set-key (kbd "M-\\") 'downcase-word)

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

;; (use-package mood-line
;;   :config
;;   (mood-line-mode))

;; (use-package doom-modeline
;;   :ensure t
;;   :init (doom-modeline-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertico
;; Completion Framework
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-indexed
                                vertico-flat
                                vertico-grid
                                vertico-mouse
                                vertico-quick
                                vertico-buffer
                                vertico-repeat
                                vertico-reverse
                                vertico-directory
                                vertico-multiform
                                vertico-unobtrusive))
  :defer t
  :custom
  (vertico-cycle t)
  ;; (vertico-multiform-categories
  ;;  '((buffer flat (vertico-cycle . t))))
  (vertico-multiform-commands
   '((consult-imenu buffer indexed)
     (consult-grep indexed)
     (consult-ripgrep indexed)
     (consult-find buffer indexed)
     (consult-fd buffer indexed)
	 (affe-find buffer indexed)
	 (affe-grep buffer indexed)
     (find-file flat (vertico-cycle .t ))
	 (execute-extended-command flat (vertico-cycle . t))))
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  :bind (("M-r" . vertico-repeat)
         (:map vertico-map
	           ("RET" . vertico-directory-enter)
	           ("DEL" . vertico-directory-delete-char)
	           ("M-DEL" . vertico-directory-delete-word)))
  :hook ((minibuffer-setup . vertico-repeat-save)
         (rfn-eshadow-update-overlay . vertico-directory-tidy)))

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

;; (global-set-key (kbd "C-x C-p") 'project-find-file)

(use-package consult-project-extra
  :straight (consult-project-extra :type git :host github :repo "Qkessler/consult-project-extra")
  :bind
  (("C-x C-p" . consult-project-extra-find)
   ("C-x C-\\" . consult-project-extra-find-other-window)))

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
  :config
  (setq embark-help-key rs/help-key)
  (setq prefix-help-command #'embark-prefix-help-command)
  (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  (add-to-list 'display-buffer-alist
	           '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		         nil
		         (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Undo
;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package undo-fu-session
;;   :defer 0
;;   :init
;;   (undo-fu-session-global-mode))

;; (use-package vundo
;;   :commands (vundo vundo-backward vundo-forward)
;;   :bind (("C-M-u" . vundo)
;; 	     :map vundo-mode-map
;;          ("l" . vundo-forward)
;; 	     ("h" . vundo-backward)
;; 	     ("j" . vundo-next)
;; 	     ("k" . vundo-previous)
;; 	     ("<right>" . vundo-forward)
;; 	     ("<left>" . vundo-backward)
;; 	     ("<up>" . vundo-next)
;; 	     ("<down>" . vundo-previous)
;; 	     ("C-a" . vundo-stem-root)
;; 	     ("C-e" . vundo-stem-end)
;; 	     ("RET" . vundo-confirm)
;; 	     ("q" . vundo-quit)
;; 	     ("C-g" . vundo-quit))
;;   :config
;;   (setq vundo-compact-display t))

(use-package undo-tree
  :diminish
  :bind (("C-M-u" . undo-tree-visualize)
         ("M-_" . undo-tree-redo)
         ("M--" . go-back-to-last-edit)
         :map undo-tree-mode-map
         (("C-x u" . nil)))
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-enable-undo-in-region t)
  ;; (undo-tree-auto-save-history nil)
  (undo-tree-history-directory-alist `((".*" . ,(expand-file-name "undo" rs/emacs-dir))))
  ;; (setq undo-tree-visualizer-relative-timestamps nil)
  ;; (setq undo-tree-visualizer-timestamps t)
  :config
  (defun go-back-to-last-edit ()
    "Jump back to the last change in the current buffer."
    (interactive)
    (ignore-errors
      (let ((inhibit-message t))
        (undo-tree-undo)
        (undo-tree-redo))))

  (defun undo-tree-split-side-by-side (original-function &rest args)
    "Split undo-tree side-by-side"
    (let ((split-height-threshold nil)
          (split-width-threshold 0))
      (apply original-function args)))

  (advice-add 'undo-tree-visualize :around #'undo-tree-split-side-by-side)
  :init
  (global-undo-tree-mode))


;;;;;;;;;;;;;;;;;;;;;;;;
;; ENV
;;;;;;;;;;;;;;;;;;;;;;;;

(defun rs/update-env ()
  (interactive)
  (shell-command (concat "zsh -ic printenv > " rs/env-file)))

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

;; Select the window you create
(defun split-follow (&rest _arg)
  "Advice to follow a function which spawn a window."
  (other-window 1))

(advice-add 'split-window-below :after #'split-follow)
(advice-add 'split-window-right :after #'split-follow)

(use-package ace-window
  :bind (("C-x C-o" . ace-window)))

(use-package windmove
  :straight (:type built-in)
  ;; :bind (("C-<right>" . windmove-right)
  ;;        ("C-<left>" . windmove-left)
  ;;        ("C-<down>" . windmove-down)
  ;;        ("C-<up>" . windmove-up))
  :bind (("C-l" . windmove-right)
         ("C-h" . windmove-left)
         ("C-j" . windmove-down)
         ("C-k" . windmove-up)
         ("C-S-l" . windmove-swap-states-right)
         ("C-S-h" . windmove-swap-states-left)
         ("C-S-j" . windmove-swap-states-down)
         ("C-S-k" . windmove-swap-states-up))
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
  ;; (dired-listing-switches "-Alh")
  (dired-listing-switches "-AlhcF --group-directories-first")
  (dired-auto-revert-buffer t))

(use-package async
  :after dired
  :init (dired-async-mode t))

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
  :after dired
  :bind (:map dired-mode-map
              ([remap dired-find-file] . dired-single-buffer)
              ([remap dired-mouse-find-file-other-window] . dired-single-buffer-mouse)
              ([remap dired-up-directory] . dired-single-up-directory)))

(use-package dired-open
  :after dired
  :bind (:map dired-mode-map
              ("o" . dired-open-xdg)))

;; (use-package dired-collapse
;;   :after dired
;;   :hook (dired-mode . dired-collapse-mode))

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :bind (:map dired-mode-map
              ("H" . dired-hide-dotfiles-mode)))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("TAB" . dired-subtree-toggle)
              ("M-TAB" . dired-subtree-cycle)
              ("<backtab>" . dired-subtree-remove)))

(use-package dired-git-info
  :pin "gnu"
  :after dired
  :custom (dgi-auto-hide-details-p nil)
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;;narrow dired to match filter
(use-package dired-narrow
  :after dired
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Terminal
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *") ;; Set this to match your custom shell prompt
  (setq vterm-shell "zsh") ;; Set this to customize the shell to launch
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
  (setq password-cache-expiry nil)
  (put 'temporary-file-directory 'standard-value '("/tmp")))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Git
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :bind (("C-x g" . magit)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Snippet
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package yasnippet
  :after corfu
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package doom-snippets
  :after yasnippet
  :straight (doom-snippets :type git :host github :repo "doomemacs/snippets" :files ("*.el" "*")))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree Sitter
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treesit-auto
  :defer 1
  :config
  (global-treesit-auto-mode)
  (let ((astro-recipe (make-treesit-auto-recipe
                       :lang 'astro
                       :ts-mode 'astro-ts-mode
                       :url "https://github.com/virchau13/tree-sitter-astro"
                       :revision "master"
                       :source-dir "src")))
    (add-to-list 'treesit-auto-recipe-list astro-recipe)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming Language
;;;;;;;;;;;;;;;;;;;;;;;;

;; C
(setq-default c-basic-offset 4)
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

(use-package nix-mode
  :commands (nix-mode)
  :ensure t)

;; Python
(use-package python-mode
  :straight (:type built-in)
  :mode ("\\.py\\'" . python-ts-mode))

(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "<backspace>") 'delete-backward-char)
            (define-key python-mode-map (kbd "<S-tab>") 'python-indent-dedent-line)))

(use-package lua-mode
  :mode ("\\.lua\\'" . lua-mode))

;; Powershell
(use-package powershell
  :commands (powershell powershell-mode))

;; JS/JSX
(use-package rjsx-mode
  :mode (("\\.js\\'" . rjsx-mode)
         ("\\.mjs\\'" . rjsx-mode)))

;; Astro
(use-package astro-ts-mode
  :mode ("\\.astro\\'" . astro-ts-mode))

;; Rust
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

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

(setq org-sources (flatten-list (append (list rs/org-dir)
                                        (list rs/phd-dir))))

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
         ("C-c C-l" . org-insert-link-global)
         ("C-c s" . rs/org-search)
         ("C-c f" . rs/org-find-file))
  :init
  (setq org-directory rs/org-dir)
  (defun rs/org-search ()
    "Search org directory using consult-ripgrep. With live-preview."
    (interactive)
    (consult-ripgrep org-sources))
  (defun rs/org-find-file ()
    "Search org directory using consult-fd. With live-preview."
    (interactive)
    (consult-fd org-sources))
  :config
  (progn
    (setq org-log-done 'time)
    (setq org-src-fontify-natively t)
    (setq org-use-speed-commands t)
    ;; (setq org-startup-folded t)
    (setq org-hide-block-startup t)
    (setq org-capture-templates
	      `(("t" "Todo [inbox]" entry (file+headline ,(expand-file-name "gtd/inbox.org" rs/org-dir) "Tasks") "* TODO %i%?")
	        ("T" "Tickler" entry (file+headline ,(expand-file-name "gtd/tickler.org" rs/org-dir) "Tickler") "* %i%? \n %^t")
            ("m" "Email Workflow")
            ("mf" "Follow Up" entry (file+olp ,(expand-file-name "gtd/mail.org" rs/org-dir) "Follow Up")
             "* TODO Follow up with %:fromname on %:subject\n%a\nSCHEDULED:%t\n\n%i")
            ("mr" "Read Later" entry (file+olp ,(expand-file-name "gtd/mail.org" rs/org-dir) "Read Later")
             "* TODO Read %:subject\n%a\nSCHEDULED:%t\n\n%i")))
    (setq org-refile-targets
	      `((,(expand-file-name "gtd/inbox.org" rs/org-dir) :maxlevel . 3)
	        (,(expand-file-name "gtd/someday.org" rs/org-dir) :level . 1)
            (,(expand-file-name "gtd/gtd.org" rs/org-dir) :level . 1)
	        (,(expand-file-name "gtd/tickler.org" rs/org-dir) :maxlevel . 2)))

    (setq org-todo-keywords
          '(;; Sequence for TASKS
            ;; TODO means it's an item that needs addressing
            ;; INPROGRESS means it's currently being worked on
            ;; WAITING means it's dependent on something else happening
            ;; DELEGATED means someone else is doing it and I need to follow up with them
            ;; ASSIGNED means someone else has full, autonomous responsibility for it
            ;; CANCELLED means it's no longer necessary to finish
            ;; DONE means it's complete
            (sequence "TODO(t@/!)" "INPROGRESS(i@/!)" "WAITING(w@/!)" "DELEGATED(e@/!)" "|" "CANCELLED(x@/!)" "DONE(d@/!)" )
            ;; Sequence for EVENTS
            ;; VISIT means that there is something you would physically like to do, no dates associated
            ;; DIDNOTGO means the event was cancelled or I didn't go
            ;; MEETING means a real time meeting, i.e. at work, or on the phone for something official
            ;; VISITED means the event took place and is no longer scheduled
            (sequence "VISIT(v@/!)" "|" "DIDNOTGO(z@/!)" "MEETING(m@/!)" "VISITED(y@/!)")
            )) ; Task has been cancelled
    ;; (setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))

    ;; The GTD part of this config is heavily inspired by
    ;; https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
    ;; (setq org-agenda-files
	;;       (apply 'append
    ;;              (mapcar (lambda (path) (expand-file-name path rs/org-dir))
	;; 	                 '("/inbox.org"
	;; 	                   "/gtd/gtd.org"
	;; 	                   "/gtd/inbox.org"
	;; 	                   "/gtd/tickler.org"))
    ;;              '()))

    (setq org-agenda-files (apply 'append
			                      (mapcar
			                       (lambda (directory)
				                     (directory-files-recursively
				                      directory org-agenda-file-regexp))
			                       `(,(expand-file-name "gtd" rs/org-dir)))))

    (setq org-agenda-custom-commands
	      '(("@" "Contexts"
	         ((tags-todo "@email"
			             ((org-agenda-overriding-header "Emails")))
	          (tags-todo "@phone"
			             ((org-agenda-overriding-header "Phone")))))
            ("p" "Priority"
             ((tags-todo "PRIORITY=\"A\" -IGNORED"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "IGNORED")))
                          (org-agenda-overriding-header "🟥 High priority:")))
              (tags-todo "RESEARCH -IGNORED"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "IGNORED")))
                          (org-agenda-overriding-header "⁖ RESEARCH:")))
              (tags-todo "PRIORITY=\"B\" -IGNORED"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "IGNORED")))
                          (org-agenda-overriding-header "🟧 Medium priority:")))
              (tags-todo "PRIORITY=\"C\" -IGNORED"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "IGNORED")))
                          (org-agenda-overriding-header "🟨 Low-priority:")))
              (tags-todo "UCARE -IGNORED"
                         ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo '("DONE" "IGNORED")))
                          (org-agenda-overriding-header "● UCARE:")))
              (agenda "")
              (alltodo "")))

            (" " "Agenda"
             ((agenda ""
                      ((org-agenda-span 'week)
                       (org-deadline-warning-days 365)))
              (todo "INPROGRESS"
                    ((org-agenda-overriding-header "In Progress")
                     (org-agenda-files `(,(expand-file-name "gtd/gtd.org" org-directory)))))
              (alltodo ""
                       ((org-agenda-overriding-header "Inbox")
                        (org-agenda-files `(,(expand-file-name "gtd/inbox.org" org-directory)))))
              (todo "TODO"
                    ((org-agenda-overriding-header "Active Projects")
                     (org-agenda-files `(,(expand-file-name "gtd/gtd.org" org-directory)))
                     (org-agenda-skip-function #'rs/skip-projects)))))))

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

(defun rs/capture-mail-follow-up (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "mf"))

(defun rs/capture-mail-read-later (msg)
  (interactive)
  (call-interactively 'org-store-link)
  (org-capture nil "mr"))

(global-set-key (kbd "C-c <tab>") #'rs/org-capture-inbox)
(global-set-key (kbd "C-c SPC") #'rs/org-agenda)

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
  :init
  (setq org-bullets-face-name "Inconsolata-12")
  (setq org-bullets-bullet-list
        '("·" ":" "⁖" "⁘" "⁙" "○"))
  :hook (org-mode . org-bullets-mode))

(use-package org-roam
  ;; :requires (org)
  :commands (org-roam-buffer-toggle
             org-roam-node-insert
             org-roam-node-find
             org-roam-ref-find
             org-roam-show-graph
             org-roam-capture
             rs/org-roam-rg-search)
  :bind (("C-c n r" . org-roam-buffer-toggle)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-ref-find)
         ("C-c n g" . org-roam-show-graph)
         ("C-c n n" . org-roam-capture)
         ("C-c n s" . rs/org-roam-rg-search))
  ;; :custom
  ;; (org-roam-directory (expand-file-name "brain/" rs/org-dir))
  ;; (org-roam-database-connector 'sqlite-builtin)
  ;; (org-roam-db-gc-threshold rs/default-gc-cons-threshold)
  ;; (org-id-link-to-org-use-id t)
  ;; (org-roam-completion-everywhere t)
  ;; (org-roam-db-autosync-mode +1)
  :init
  (setq org-roam-directory (expand-file-name "brain/" rs/org-dir))
  (setq org-roam-database-connector 'sqlite-builtin)
  (setq org-roam-db-gc-threshold rs/default-gc-cons-threshold)
  (setq org-id-link-to-org-use-id t)
  (setq org-roam-completion-everywhere t)
  (setq org-roam-db-autosync-mode +1)
  (defun rs/org-roam-rg-search ()
    "Search org-roam directory using consult-ripgrep. With live-preview."
    (interactive)
    (consult-ripgrep org-roam-directory))
  :config
  (add-hook 'org-roam-mode-hook #'turn-on-visual-line-mode)
  (setq org-roam-capture-templates
        '(("m" "main" plain
           "%?"
           :if-new (file+head "main/${slug}.org"
                              "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "refs/${slug}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("p" "private" plain "%?"
           :if-new
           (file+head "private/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("a" "article" plain "%?"
           :if-new
           (file+head "articles/${slug}.org" "#+title: ${title}\n#+filetags: :article:\n")
           :immediate-finish t
           :unnarrowed t)
          ("n" "literature note" plain
           "%?"
           :target
           (file+head
            "refs/${citar-citekey}.org"
            "#+title: ${title}\n#+authors: ${citar-author}\n#+created: %U\n#+last_modified: %U\n\n")
           :unnarrowed t)))

  ;; tag new notes as draft
  (defun rs/tag-new-node-as-draft ()
    (org-roam-tag-add '("draft")))
  (add-hook 'org-roam-capture-new-node-hook #'rs/tag-new-node-as-draft)

  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  
  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag))))

(use-package org-modern
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;

;; Diminish
(use-package diminish)

;; Editorconfig
(use-package editorconfig
  :defer 1
  :diminish
  :config
  (editorconfig-mode 1))

;; Anzy
(use-package anzu
  :defer 3
  :diminish
  :custom (anzu-search-threshold 333)
  :bind (([remap query-replace]        . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         ([remap isearch-query-replace]        . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :config
  (global-anzu-mode +1))

;; Mouse
(use-package mouse
  :defer 1
  :ensure nil
  :straight (:type built-in)
  :custom
  ;; GUI only
  ;; middle click paste at point like in console, not at mouse point
  (mouse-yank-at-point t)
  :config
  (global-set-key (kbd "<mouse-4>") 'previous-line)
  (global-set-key (kbd "<mouse-5>") 'next-line)
  (global-set-key (kbd "<mouse-6>") 'backward-char-same-line)
  (global-set-key (kbd "<mouse-7>") 'forward-char-same-line))

;; Unique Buffer
(use-package uniquify
  :ensure nil
  :straight (:type built-in)
  :custom (uniquify-buffer-name-style 'forward))

;; Commenter
(use-package evil-nerd-commenter
  :bind ("M-;" . evilnc-comment-or-uncomment-lines))

;; Paredit
(use-package paredit
  :bind (:map paredit-mode-map
              ("M-d" . nil)
              ("C-M-u" . nil))
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
  :bind (:map copilot-completion-map
                ("<tab>" . copilot-accept-completion)
                ("TAB" . copilot-accept-completion)))

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
;; (use-package hl-line
;;   :defer 2
;;   :ensure nil
;;   :config
;;   ;; (set-face-attribute 'hl-line nil :inherit nil :background nil :underline t)
;;   (set-face-attribute 'hl-line nil :underline nil)
;;   (global-hl-line-mode))

;; Multiple Cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-M-SPC" . set-rectangular-region-anchor)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)
         ("C-\"" . mc/skip-to-next-like-this)
         ("C-:" . mc/skip-to-previous-like-this)))

;; Which Key
(use-package which-key
  :defer 1
  :diminish which-key-mode
  :custom
  (which-key-paging-prefixes nil)
  (which-key-show-docstrings nil)
  (which-key-side-window-location 'bottom)
  (which-key-sort-order 'which-key-description-order)
  (which-key-idle-delay 1)
  :config
  (which-key-mode))

;; Trailing Whitespace
(use-package ws-butler
  :diminish
  :hook (prog-mode . ws-butler-mode))

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
  ("C-c C-f" . crux-recentf-find-file))
;; ("C-c C-a" . move-beginning-of-line)
;; ("C-a" . crux-move-beginning-of-line)

;; Rainbow
(use-package rainbow-mode
  :hook (prog-mode . rainbow-mode))

;; (use-package rainbow-delimiters
;;   :hook (prog-mode . rainbow-delimiters-mode))

;; PDF
(use-package pdf-tools
  :ensure nil
  :straight nil
  :load-path "/nix/store/089m60n5n3l4vkz55ky2kqm9djjf1n52-emacs-pdf-tools-20230322.1541/share/emacs/site-lisp/elpa/pdf-tools-20230322.1541"
  :magic ("%PDF" . pdf-view-mode)
  :config
  (pdf-tools-install :no-query))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Text mode
;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'text-mode-hook (lambda ()
                            (electric-indent-local-mode -1)
                            (setq-local truncate-lines nil)
                            (setq use-hard-newlines -1)))

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

(add-hook 'message-mode-hook (lambda ()
                               (electric-indent-local-mode -1)
                               (setq-local truncate-lines nil)
                               (setq use-hard-newlines -1)))

(setq message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-f-is-eval t
      ;; message-sendmail-extra-arguments '("--read-envelope-from")
      message-sendmail-envelope-from 'header
      mail-specify-envelope-from 't
      mail-envelope-from 'header
      sendmail-program (rs/executable-ensure "msmtp")
      ;; Setup default OPENPGP key
      mml-secure-openpgp-sign-with-sender t)

;; https://jherrlin.github.io/posts/emacs-mu4e/
(defun rs/sign-or-encrypt-message ()
  (let ((answer (read-from-minibuffer "Sign or encrypt?\nEmpty to do nothing.\n[s/e]: ")))
    (cond
     ((string-equal answer "s") (progn
                                  (message "Signing message.")
                                  (mml-secure-message-sign-pgpmime)))
     ((string-equal answer "e") (progn
                                  (message "Encrypt and signing message.")
                                  (mml-secure-message-encrypt-pgpmime)))
     (t (progn
          (message "Dont signing or encrypting message.")
          nil)))))

(add-hook 'message-send-hook 'rs/sign-or-encrypt-message)

;; https://emacs.stackexchange.com/a/41176
(defun rs/confirm-empty-subject ()
  "Allow user to quit when current message subject is empty."
  (or (message-field-value "Subject")
      (yes-or-no-p "Really send without Subject? ")
      (keyboard-quit)))

(add-hook 'message-send-hook #'rs/confirm-empty-subject)

(use-package notmuch
  :commands (notmuch)
  :bind (("C-c m n" . notmuch)
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
                                   :sort-order newest-first)
                            (:name "sent"
                                   :query "tag:sent"
                                   :key ,(kbd "t"))
                            (:name "all mail"
                                   :query "*"
                                   :key ,(kbd "a"))))
  :config
  (turn-on-gnus-dired-mode)
  (add-hook 'notmuch-hello-mode-hook
            (lambda () (display-line-numbers-mode 0))))

(use-package ol-notmuch
  :after (notmuch org-mode))

;; (use-package messages-are-flowing
;;   ;; :hook (message-mode-hook . messages-are-flowing-use-and-mark-hard-newlines)
;;   :config
;;   (add-hook 'message-mode-hook #'messages-are-flowing-use-and-mark-hard-newlines))

(use-package mu4e
  :ensure nil
  :straight nil
  :defer 20
  :load-path "/nix/store/02sl6pcdlfn9gislqya3fhjdpb9fp90s-emacs-mu4e-1.10.7/share/emacs/site-lisp/elpa/mu4e-1.10.7"
  :commands (mu4e)
  :bind (("C-c m m" . mu4e))
  :init
  (defun rs/store-link-to-mu4e-query ()
    (interactive)
    (let ((org-mu4e-link-query-in-headers-mode t))
      (call-interactively 'org-store-link)))
  
  ;; https://emacs.stackexchange.com/questions/10884/mu4e-multiple-accounts
  (defun rs/mu4e-set-account ()
    ;; (interactive)
    "Set the account for composing a message."
    (let* ((account
            (if mu4e-compose-parent-message
                (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
                  (string-match "/\\(.*?\\)/" maildir)
                  (match-string 1 maildir))
              (completing-read (format "Compose with account: (%s) "
                                       (mapconcat #'(lambda (var) (car var))
                                                  rs/mu4e-account-alist "/"))
                               (mapcar #'(lambda (var) (car var)) rs/mu4e-account-alist)
                               nil t nil nil (caar rs/mu4e-account-alist))))
           (account-vars (cdr (assoc account rs/mu4e-account-alist))))
      (if account-vars
          (mapc #'(lambda (var)
                    (set (car var) (cadr var)))
                account-vars)
        (error "No email account found"))))

  ;; ask for account when composing mail
  (defvar rs/mu4e-account-alist
    '(("rs"
       (user-mail-address  "rs@rs.ht")
       (user-full-name     "Ray Andrew")
       (mu4e-sent-folder   "/rs/Sent")
       (mu4e-drafts-folder "/rs/Drafts")
       (mu4e-trash-folder  "/rs/Trash")
       (mu4e-refile-folder "/rs/Archive"))
      ("uchicago"
       (user-mail-address  "rayandrew@uchicago.edu")
       (user-full-name     "Ray Andrew")
       (mu4e-sent-folder   "/uchicago/Sent")
       (mu4e-drafts-folder "/uchicago/Drafts")
       (mu4e-trash-folder  "/uchicago/Trash")
       (mu4e-refile-folder "/uchicago/Archive"))
      ("raydreww"
       (user-mail-address  "raydreww@gmail.com")
       (user-full-name     "Ray Andrew")
       (mu4e-sent-folder   "/raydreww/[Gmail]/Sent Mail")
       (mu4e-drafts-folder "/raydreww/[Gmail]/Drafts")
       (mu4e-trash-folder  "/raydreww/[Gmail]/Trash")
       (mu4e-refile-folder "/raydreww/Archive"))))
  :custom
  (mu4e-view-show-images t)
  (mu4e-view-show-addresses 't)

  ;; Composing mail
  (mu4e-compose-dont-reply-to-self t)

  (mu4e-compose-format-flowed t) 
  (mu4e-view-use-gnus t)
  (mu4e-user-mail-address-list
   (mapcar (lambda (account) (cadr (assq 'user-mail-address account)))
           rs/mu4e-account-alist))
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (mu4e-change-filenames-when-moving t)
  ;; Refresh mail using isync every 5 minutes
  (mu4e-update-interval (* 5 60))
  (mu4e-get-mail-command "mbsync -a")
  (mu4e-maildir "~/.mail")
  (mu4e-maildir-shortcuts
   '(;; personal
     ("/rs/Inbox" . ?p)
     ("/rs/Sent" . ?P)
     ("/rs/Archive" . ?o)
     ("/rs/Drafts" . ?O)
     ;; gmail
     ("/raydreww/Inbox" . ?g)
     ("/raydreww/[Gmail]/Sent Mail" . ?G)
     ("/raydreww/Archive" . ?h)
     ("/raydreww/[Gmail]/Drafts" . ?H)
     ;; uchicago
     ("/uchicago/Inbox" . ?u)
     ("/uchicago/Sent" . ?U)
     ("/uchicago/Archive" . ?8)
     ("/uchicago/Drafts" . ?*)))
  (mu4e-view-actions
   '(("sMark as spam" . mu4e-view-register-msg-as-spam)
     ("hMark as ham" . mu4e-view-register-msg-as-ham)
	 ("bView in browser" . mu4e-action-view-in-browser)
	 ("capture message" . mu4e-action-capture-message)
	 ("tShow this thread" . mu4e-action-show-thread)
     ("fFollow up (C)" . rs/capture-mail-follow-up)
     ("rRead Later (C)" . rs/capture-mail-read-later)))
  (mu4e-headers-actions mu4e-view-actions)
  (message-kill-buffer-on-exit t)
  
  :config
  (turn-on-gnus-dired-mode)
  ;; (add-hook 'mu4e-compose-mode-hook (lambda () (use-hard-newlines -1)))
  (add-hook 'mu4e-compose-pre-hook 'rs/mu4e-set-account)
  (add-to-list 'mu4e-bookmarks
               '(:name "All Inboxes"
                        :query "maildir:/rs/Inbox OR maildir:/uchicago/Inbox OR maildir:/raydreww/Inbox"
                        :key ?i))
  (require 'mu4e-org)
  (mu4e t))

(with-eval-after-load "mm-decode"
  (add-to-list 'mm-discouraged-alternatives "text/html")
  (add-to-list 'mm-discouraged-alternatives "text/richtext"))

(setq rs/mu4e-inbox-query
        "(maildir:/rs/Inbox OR maildir:/uchicago/Inbox OR maildir:/raydreww/Inbox) AND flag:unread")


(use-package mu4e-alert
  :after mu4e
  :config
  ;; Show unread emails from all inboxes
  (setq mu4e-alert-interesting-mail-query rs/mu4e-inbox-query)

  ;; Show notifications for mails already notified
  (setq mu4e-alert-notify-repeated-mails nil)
  (mu4e-alert-enable-notifications)

  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

  (setq mu4e-alert-set-default-style 'libnotify)
  (add-hook 'after-init-hook #'mu4e-alert-enable-notifications))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Browser
;;;;;;;;;;;;;;;;;;;;;;;;

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
;; Others
;;;;;;;;;;;;;;;;;;;;;;;;

(use-package leetcode
  :commands (leetcode)
  :custom
  (leetcode-prefer-language "python3")
  ;; (leetcode-prefer-language "cpp")
  (leetcode-prefer-sql "mysql")
  (leetcode-save-solutions t)
  (leetcode-directory "~/Code/leetcode"))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Epilogue
;;;;;;;;;;;;;;;;;;;;;;;;

(put 'downcase-region 'disabled nil)
(global-unset-key (kbd "C-z"))

