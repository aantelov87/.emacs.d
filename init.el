;; Turn off mouse interface early in startup to avoid momentary display

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

(setq package-user-dir
      (expand-file-name "vendor" user-emacs-directory))

;; Add package repos
(add-to-list 'package-archives '("melpa" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives ' ("gnu" . "https://elpa.gnu.org/packages/") t)

(defvar my-packages '(better-defaults paredit idle-highlight-mode ido-ubiquitous
                                      find-file-in-project magit smex))


(package-initialize)

; check for new packages (package versions)
(message "%s" "Emacs is now refreshing its package database...")
(package-refresh-contents)
(message "%s" " done.")
                                        ; install the missing packages

(message "%s" "Emacs is installing packages from repositories...")
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
(message "%s" " done.")



;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq core-dir
      (expand-file-name "core" user-emacs-directory))

(setq settings-dir
      (expand-file-name "settings" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path settings-dir)
(add-to-list 'load-path core-dir)

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; Are we on a mac?
(setq is-mac (equal system-type 'darwin))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Setup environment variables from the user's shell.
;;(when is-mac
;;  (require-package 'exec-path-from-shell)
;;  (exec-path-from-shell-initialize))

