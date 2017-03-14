;; Turn off mouse interface early in startup to avoid momentary display

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

(setq package-user-dir
      (expand-file-name "vendor" user-emacs-directory))

;; Add package repos
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives ' ("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)

;; Load paths
(add-to-list 'load-path (expand-file-name "pkg" user-emacs-directory))

(require 'cl)

; check for new packages (package versions)
(when (not package-archive-contents)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done."))

(defvar my-packages '(better-defaults
                      paredit

                      ;; Emacs extensions
                      smex
                      idle-highlight-mode
                      ido-ubiquitous

                      ;; Project organization
                      find-file-in-project
                      
                      ;; Errors reporting
                      flycheck
                      
                      ;; Version Control
                      magit ;; git

                      ;; Programming language
                      js2-mode js2-refactor  tide ;; Javascript and typescript 
                      css-mode
                      php-mode
                      go-mode go-eldoc go-autocomplete gotest;; golang
                      web-mode
                      
                      ;; Serialization language
                      protobuf-mode
                      yaml-mode
                      json-mode
                      ))

; install the missing packages

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


;; Core Libs
(require 'defaults)
(require 'defuns)

;; No splash screen please ... jeez
;; (setq inhibit-startup-message t)


;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; Keys Bindings
(require 'keys-bindings)


