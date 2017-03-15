;; Initialization
(require 'package)

(setq package-user-dir (expand-file-name "vendor" user-emacs-directory))

;; Add package repos
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)

;; Load paths
(add-to-list 'load-path (expand-file-name "pkg" user-emacs-directory))

; check for new packages (package versions)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      ;; Emacs extensions
                      smex
                      idle-highlight-mode
                      ido-ubiquitous
		      exec-path-from-shell
		      auto-complete
                      find-file-in-project
                      paredit
                      
                      ;; Errors reporting
                      flycheck
                      
                      ;; Version Control
                      magit ;; git

                      ;; Programming language
                      php-mode ;; PHP
                      go-mode go-eldoc go-autocomplete gotest ;; golang
                      web-mode scss-mode css-mode ;; HTML, CSS and JS
                      js2-mode js2-refactor  ;; JS 
                      ng2-mode typescript tide ;; Typescript && AngularJS

                      ;; Serialization language
                      protobuf-mode
                      yaml-mode
                      json json-reformat json-snatcher ;; json
                      ))

;; install the missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; No splash screen please ... jeez
 (setq inhibit-startup-message t)

;; Load core modules && defined functions
(require 'tramp)
(require 'defaults)
(require 'defuns)

;; Load modules for PHP and GOLANG
(require 'php-mode) ;; PHP

(require 'go-mode) ;; Golang
(require 'go-eldoc)
(require 'go-autocomplete)
(require 'gotest)

;; Load modules for HTML, CSS and JS
(require 'web-mode) ;; General web development

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.twig\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))


(require 'scss-mode) ;; CSS and SCSS
(require 'css-mode)

(require 'js2-mode)     ;; Javascript
(require 'js2-refactor)

(require 'typescript) ;; Typescript && Angular
(require 'tide)
(require 'ng2-mode)

;; Load modules for encoding formats (json, yaml-mode, proto)
(require 'protobuf-mode)

(require 'yaml-mode)
(require 'json)
(require 'json-reformat)
(require 'json-snatcher)

(defun js-mode-bindings ()
    "Sets a hotkey for using the json-snatcher plugin"
      (when (string-match  "\\.json$" (buffer-name))
            (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
(add-hook 'js-mode-hook 'js-mode-bindings)
(add-hook 'js2-mode-hook 'js-mode-bindings)

;; Keys Bindings
(require 'keys-bindings)
