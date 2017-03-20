(unless (fboundp 'helm-mode)
  (ido-mode t)
  (setq ido-enable-flex-matching t))

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode)
  (horizontal-scroll-bar-mode -1))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(show-paren-mode 1)
(setq-default word-wrap t)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Write backup files to own directory
;; store all backup and autosave files in the tmp dir
(setq backups-dir
      (expand-file-name ".backups" user-emacs-directory))
(setq places-dir
      (expand-file-name ".places" user-emacs-directory))

;; Write backup files to own directory
(setq backup-directory-alist
      `((".*" . ,backups-dir)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))




;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))


(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell nil
      load-prefer-newer t
      ediff-window-setup-function 'ediff-setup-windows-plain)

;; Sane

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; 120 chars is a good width.
(set-default 'fill-column 120)

(provide 'defaults)
