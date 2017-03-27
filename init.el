(setq is-mac (equal system-type 'darwin))
(set-default-font "Monaco 16")

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

					; check for new packages (package versions)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(
		      ;; Emacs extensions
		      smex

		      idle-highlight-mode
		      ido-ubiquitous
		      ido-vertical-mode
		      ido-at-point
		      auto-complete
		      find-file-in-project
		      paredit
		      markdown-mode
		      mu4e-alert
		      multiple-cursors

		      ;; Errors reporting
		      flycheck
		      flycheck-pos-tip

		      ;; Version Control
		      magit ;; git

		      ;; Programming language
		      php-mode ac-php ;; PHP
		      go-mode go-eldoc go-autocomplete gotest go-guru ;; golang
		      web-mode scss-mode css-mode ;; HTML, CSS and JS
		      js2-mode js2-refactor  ;; JS
		      ng2-mode typescript tide ;; Typescript && AngularJS

		      ;; Serialization language
		      protobuf-mode
		      yaml-mode
		      json json-reformat json-snatcher ;; json
		      ))

(when is-mac
  (add-to-list 'my-packages 'exec-path-from-shell))

;; install the missing packages
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(when is-mac
  (add-to-list 'my-packages 'exec-path-from-shell)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)
  (require 'exec-path-from-shell)
  (set-default-font "Monaco 14")
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  ;; Don't open files from the workspace in a new frame
  (setq ns-pop-up-frames nil)
  ;; Use aspell for spell checking: brew install aspell --lang=en
  (setq ispell-program-name "/usr/local/bin/aspell"))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Load core modules && defined functions
(require 'tramp)
(require 'flycheck)
(require 'flycheck-pos-tip)
(require 'defaults)
(require 'defuns)
(require 'find-file-in-project)
(require 'multiple-cursors)

(require 'epa-file)
(custom-set-variables '(epg-gpg-program	 "/usr/local/bin/gpg2"))
(epa-file-enable)

(require 'mail-client)

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

;; Git mode settings
(require 'magit)
(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

;; Completation mode
(require 'auto-complete)
(global-auto-complete-mode t)

;; Load modules for PHP and GOLANG
(require 'php-mode) ;; PHP
(add-hook 'php-mode-hook 'my-php-mode-hook)

;;Load Go-specific language syntax
(add-hook 'go-mode-hook 'go-mode-setup)

;;Load auto-complete
(ac-config-default)
(require 'auto-complete-config)
(require 'go-autocomplete)

;; If the go-guru.el file is in the load path, this will load it.
(require 'go-guru)


;; Load modules for HTML, CSS and JS
(eval-after-load "sgml-mode"
  '(progn
     (define-key html-mode-map
       [remap forward-paragraph] 'skip-to-next-blank-line)

     (define-key html-mode-map
       [remap backward-paragraph] 'skip-to-previous-blank-line)))

(require 'web-mode) ;; General web development
(add-hook 'web-mode-hook 'extension-tide-mode)

(require 'scss-mode) ;; CSS and SCSS
(require 'css-mode)

(require 'js2-mode)	;; Javascript
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)

(require 'typescript) ;; Typescript && Angular
(require 'tide)
(require 'ng2-mode)

(setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)
(add-hook 'js2-mode-hook #'setup-tide-mode)

;; Load modules for encoding formats (json, yaml-mode, proto)
(require 'protobuf-mode)
(require 'yaml-mode)
(require 'json)
(require 'json-reformat)
(require 'json-snatcher)

(add-hook 'js-mode-hook 'js-mode-bindings)
(add-hook 'js2-mode-hook 'js-mode-bindings)

(add-hook 'prog-mode-hook 'my-code-style)

;; Keys Bindings
(require 'keys-bindings)
(require 'mode-mappings)
;;; init.el ends here
