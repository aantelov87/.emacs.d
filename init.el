;;; Emacs init.el

(setq is-mac (equal system-type 'darwin))
(setq default-frame-alist
      (append default-frame-alist
              '((font . "Go Mono 16"))))

;; Initialization
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
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
                      find-file-in-project
                      markdown-mode
                      yasnippet
                      exec-path-from-shell
                      ;; Use this package for gopls
                      use-package
                      tagedit
                      paredit

                      dired-details
                      expand-region
                      ;; Errors reporting
                      flycheck
                      flycheck-pos-tip
                      ;; Version Control
                      magit ;; git
                      restclient
                      nginx-mode
                      ;; Programming language
                      go-mode ;; golang
		      go-tag
                      ;; go-play
                      gotest
                      go-gen-test
                      ;; go-snippets
                      ;; godoctor
                      go-impl
                      lsp-mode
                      lsp-ui
                      company-lsp
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
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)
  ;; Don't open files from the workspace in a new frame
  (setq ns-pop-up-frames nil)
  ;; Use aspell for spell checking: brew install aspell --lang=en
  (setq ispell-program-name "/usr/local/bin/aspell"))

;; Keep emacs Custom-settings in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

(require 'use-package)
(require 'defaults)
;; Load core modules && defined functions
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)
(exec-path-from-shell-copy-env "PATH")

(require 'tramp)
(require 'flycheck)
(require 'flycheck-pos-tip)
(require 'defuns)
(require 'find-file-in-project)
(require 'restclient)
(require 'nginx-mode)
(require 'yasnippet)
(yas-global-mode 1)

(require 'expand-region)

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

;;Load Gopls configuration took from https://github.com/golang/tools/blob/master/gopls/doc/emacs.md
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred))

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :ensure t
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; company-lsp integrates company mode completion with lsp-mode.
;; completion-at-point also works out of the box but doesn't support snippets.
(use-package company-lsp
  :ensure t
  :commands company-lsp)

;; Optional - provides snippet support.
(use-package yasnippet
  :ensure t
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(lsp-register-custom-settings
 '(("gopls.usePlaceholders" lsp-gopls-use-placeholders t)
   ("gopls.hoverKind" lsp-gopls-hover-kind)
   ("gopls.buildFlags" lsp-gopls-build-flags)
   ("gopls.env" lsp-gopls-env)))

;; If the go-guru.el file is in the load path, this will load it.
;; (require 'go-guru)
;; (require 'godoctor)
(require 'go-gen-test)
(defun my-go-gen-test-setup ()
  "My keybindings for generating go tests."
  (interactive)
  (local-set-key (kbd "C-c C-g") #'go-gen-test-dwim))

(add-hook 'go-mode-hook #'my-go-gen-test-setup)

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
