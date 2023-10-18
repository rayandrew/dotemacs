;;; early-init.el --- Emacs early init -*- lexical-binding: t; -*-

;;; Commentary:

;; Emacs 27 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens.

;;; Code:

;; Uncomment this to debug.
;; (setq init-file-debug t)
;; (setq messages-buffer-max-lines 100000)

;; These are good notes on optimizing startup performance:
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast

;; Unset `file-name-handler-alist' too (temporarily). Every file opened and
;; loaded by Emacs will run through this list to check for a proper handler for
;; the file, but during startup, it won’t need any of them.

;; (defvar file-name-handler-alist-old file-name-handler-alist)
;; (setq file-name-handler-alist nil)
;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (setq file-name-handler-alist file-name-handler-alist-old)))

;; (setq gc-cons-threshold 452653184
;;       gc-cons-percentage 0.6)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 1)

(defconst rs/default-gc-cons-threshold (* 2 1024 1024))
(defconst rs/default-gc-cons-percentage 0.1)

(defvar rs/startup/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun rs/startup/revert-file-name-handler-alist ()
  (setq file-name-handler-alist rs/startup/file-name-handler-alist))

(defun rs/startup/reset-gc ()
  (setq gc-cons-threshold rs/default-gc-cons-threshold gc-cons-percentage rs/default-gc-cons-percentage))

(add-hook 'emacs-startup-hook 'rs/startup/revert-file-name-handler-alist)
(add-hook 'emacs-startup-hook 'rs/startup/reset-gc)

;; Disable `package' in favor of `straight'.
(setq package-enable-at-startup nil)

;; If an `.el' file is newer than its corresponding `.elc', load the `.el'.
(setq load-prefer-newer noninteractive)

;; Write any customizations to a temp file so they are discarded.
(setq custom-file (make-temp-file "custom-" nil ".el"))

;; Faster to disable these here (before they've been initialized)
;; (push '(undecorated . t) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Fonts
(defun rs/get-default-font ()
  (cond
   ((eq system-type 'windows-nt) "Consolas-13")
   ((eq system-type 'gnu/linux) "Iosevka-16")
   ((eq system-type 'darwin) "Iosevka Nerd Font Mono-16")))
(add-to-list 'default-frame-alist `(font . ,(rs/get-default-font)))
;; (set-face-attribute 'default nil :family "UbuntuMono Nerd Font Mono" :height 160)
;; (set-face-attribute 'default nil :family "Iosevka Nerd Font Mono" :height 180)
;; (set-face-attribute 'default nil :family "Iosevka Comfy Wide" :height 180)

;; Give the frame basic coloring while waiting for the theme to load. The main
;; purpose of this is to not blind me when it's dark by flashing a screen full
;; of white. These colors are from doom-one.
;; (set-face-attribute 'default nil :background "#181818" :foreground "#c4ad63") ;; gruber-darker
;; (set-face-attribute 'default nil :background "#062329" :foreground "#d1b897") ;; naysayer
;; (set-face-attribute 'default nil :background "#041818" :foreground "#d3b58d") ;; naysayer
;; (set-face-attribute 'default nil :background "#052b2a" :foreground "#c5bfa3") ;; naysayer latest
;; (set-face-attribute 'default nil :background "#242424" :foreground "#f6f3e8") ;; wombat
;; (set-face-attribute 'default nil :background "#f6f7f8" :foreground "#3B5998") ;; mccarthy
;; (set-face-attribute 'default nil :background "#FFFFFF" :foreground "#505050") ;; twilight-bright
;; (set-face-attribute 'default nil :background "#FFFFFF" :foreground "#000000") ;; tango-plus
;; (set-face-attribute 'default nil :background "#ecf0f1" :foreground "#425d78") ;; flucui-light

;; Default frame settings. This is actually maximized, not full screen.
(push '(fullscreen . maximized) initial-frame-alist)
(push '(ns-transparent-titlebar . t) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

(menu-bar-mode -1) ;; Disable the menu bar
(tool-bar-mode -1) ;; Disable the tool bar
(scroll-bar-mode -1) ;; Disable the scroll bars
(setq inhibit-startup-screen t) ;; Disable splash screen

;;;;;;;;;;;;;;;;;;;;;;;;
;; Startup
;;;;;;;;;;;;;;;;;;;;;;;;

;; https://systemcrafters.net/emacs-from-scratch/cut-start-up-time-in-half/

;; (defun rs/display-startup-time ()
;;   (message "Emacs loaded in %s with %d garbage collections."
;;            (format "%.2f seconds"
;;                    (float-time
;;                     (time-subtract after-init-time before-init-time)))
;;            gcs-done))

;; (add-hook 'emacs-startup-hook #'rs/display-startup-time)

(provide 'early-init)

;;; early-init.el ends here
