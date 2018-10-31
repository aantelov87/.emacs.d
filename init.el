;;; Emacs init.el

(setq is-mac (equal system-type 'darwin))
(setq default-frame-alist
      (append default-frame-alist
              '((font . "Monaco 20"))))

;; Initialization
(require 'package)

(setq package-user-dir (expand-file-name "vendors" user-emacs-directory))

;; Add package repos
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)

(package-initialize)

;; Load paths
(add-to-list 'load-path (expand-file-name "pkg" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "config" user-emacs-directory))

;; check for new packages (package versions)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
                      ;; Emacs extensions
                      smex
                      idle-highlight-mode
                      ido-completing-read+
                      ido-vertical-mode
                      ido-at-point
                      auto-complete
                      find-file-in-project
                      paredit
		      tagedit
                      markdown-mode
                      multiple-cursors
                      yasnippet
                      exec-path-from-shell
                      dired-details
                      expand-region
                      ;; Errors reporting
                      flycheck
                      flycheck-pos-tip
                      ;; Ctags
                      ctags-update
                      ;; Version Control
                      magit ;; git
                      ;; Programming language
                      php-mode ac-php ;; PHP
                      go-mode go-eldoc go-autocomplete gotest go-add-tags go-rename go-guru ;; golang
                      dart-mode
                      web-mode scss-mode css-mode ;; HTML, CSS

                      ;; Serialization language
                      protobuf-mode
                      yaml-mode
                      json json-reformat json-snatcher ;; json
                      ))
;; install the missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(when is-mac
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)
  (setq default-frame-alist
        (append default-frame-alist
                '((font . "Monaco 20"))))
  ;; Don't open files from the workspace in a new frame
  (setq ns-pop-up-frames nil)
  ;; Use aspell for spell checking: brew install aspell --lang=en
  (setq ispell-program-name "/usr/local/bin/aspell"))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)


;; Load core modules && defined functions
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PATH")

(require 'tramp)
(require 'flycheck)
(require 'flycheck-pos-tip)
(require 'defaults)
(require 'defuns)
(require 'find-file-in-project)
(require 'multiple-cursors)

(require 'yasnippet)
(yas-global-mode 1)

(require 'expand-region)

;; Dired element
(require 'epa-file)
(custom-set-variables '(epg-gpg-program "gpg2"))
(epa-file-enable)

(global-flycheck-mode 1)
(setq flycheck-checker-error-threshold 20000)
(eval-after-load 'flycheck
  '(custom-set-variables
    '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

;; Find files in project
(autoload 'find-file-in-project "find-file-in-project" nil t)
(autoload 'find-file-in-project-by-selected "find-file-in-project" nil t)
(autoload 'find-directory-in-project-by-selected "find-file-in-project" nil t)
(autoload 'ffip-show-diff "find-file-in-project" nil t)
(autoload 'ffip-save-ivy-last "find-file-in-project" nil t)
(autoload 'ffip-ivy-resume "find-file-in-project" nil t)
(setq-local ffip-prune-patterns '("*/.git/*" "*/node_modules/*" "*/.DS_Store/*" "*/bower_components/*"))
(setq-local ffip-filename-rules
            '(;; first, search by the original file name
              ffip-filename-identity
              ;; second, apply either below rule
              (ffip-filename-dashes-to-camelcase ffip-filename-camelcase-to-dashes)))

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

;; Dired
(require 'setup-dired)

;; Git mode settings
(require 'magit)
(require 'setup-magit)
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; Use ido everywhere
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)

;; Load modules for PHP and GOLANG
;; (require 'phpfmt)
;; (require 'php-mode) ;; PHP
(add-hook 'php-mode-hook 'my-php-mode-hook)
(add-hook 'before-save-hook 'phpfmt-before-save)

;;Load Go-specific language syntax
(add-hook 'go-mode-hook 'go-mode-setup)

;;Load auto-complete
;; Completation mode
(require 'auto-complete)
(require 'go-autocomplete)
(require 'auto-complete-config)
(ac-config-default)

;; If the go-guru.el file is in the load path, this will load it.
(require 'go-guru)

;; Dart mode configuration
(setq dart-format-on-save t)
(add-hook 'dart-mode-hook 'flycheck-mode)

;; Load modules for HTML, CSS and JS
(eval-after-load "sgml-mode"
  '(progn
     (define-key html-mode-map
       [remap forward-paragraph] 'skip-to-next-blank-line)

     (define-key html-mode-map
       [remap backward-paragraph] 'skip-to-previous-blank-line)))

(require 'web-mode) ;; General web development
(require 'setup-html)

(require 'scss-mode) ;; CSS and SCSS
(require 'css-mode)

;; Load modules for encoding formats (json, yaml-mode, proto)
(require 'protobuf-mode)
(require 'yaml-mode)
(require 'json)
(require 'json-reformat)
(require 'json-snatcher)

(require 'coding-hook)


;; Keys Bindings
(require 'keys-bindings)
(require 'mode-mappings)
;;; init.el ends here
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
