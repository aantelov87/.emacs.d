(setq is-mac (equal system-type 'darwin))
(set-default-font "Monaco 16")

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
		      ido-vertical-mode
		      ido-at-point
		      exec-path-from-shell
		      auto-complete
                      find-file-in-project
                      paredit
		      dired-details
		      markdown-mode

                      ;; Errors reporting
                      flycheck
                      
                      ;; Version Control
                      magit ;; git

                      ;; Programming language
                      php-mode ;; PHP
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
  (add-to-list 'my-packages 'exec-path-from-shell)
  (setq mac-option-modifier 'super)
  (setq mac-command-modifier 'meta)
  (setq ns-function-modifier 'hyper)

  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH")
  ;; Ignore .DS_Store files with ido mode
;;  (add-to-list 'ido-ignore-files "\\.DS_Store")

  ;; Don't open files from the workspace in a new frame
  (setq ns-pop-up-frames nil)

  ;; Use aspell for spell checking: brew install aspell --lang=en
  (setq ispell-program-name "/usr/local/bin/aspell"))

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

;; Smart M-x is smart
(require 'smex)
(smex-initialize)

(require 'magit)

(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)


;; Make dired less verbose
(require 'dired-details)
(setq-default dired-details-hidden-string "--- ")
(dired-details-install)

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(ido-ubiquitous-use-new-completing-read webjump 'webjump)
(ido-ubiquitous-use-new-completing-read yas/expand 'yasnippet)
(ido-ubiquitous-use-new-completing-read yas/visit-snippet-file 'yasnippet)

(require 'auto-complete)
(global-auto-complete-mode t)
;; Load modules for PHP and GOLANG
(require 'php-mode) ;; PHP

(add-hook 'php-mode-hook 'my-php-mode-hook)
(defun my-php-mode-hook ()
  (setq indent-tabs-mode t)
  (let ((my-tab-width 8))
    (setq tab-width my-tab-width)
    (setq c-basic-indent my-tab-width)
    (set (make-local-variable 'tab-stop-list)
         (number-sequence my-tab-width 200 my-tab-width))))

(require 'go-mode) ;; Golang

;; Define function to call when go-mode loads
(defun my-go-mode-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save) ; gofmt before every save
  (setq gofmt-command "goimports")                ; gofmt uses invokes goimports
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))

  ;; guru settings
  (go-guru-hl-identifier-mode)                    ; highlight identifiers
  
  ;; Key bindings specific to go-mode
  (local-set-key (kbd "M-.") 'godef-jump)         ; Go to definition
  (local-set-key (kbd "M-*") 'pop-tag-mark)       ; Return from whence you came
  (local-set-key (kbd "M-p") 'compile)            ; Invoke compiler
  (local-set-key (kbd "M-P") 'recompile)          ; Redo most recent compile cmd
  (local-set-key (kbd "M-]") 'next-error)         ; Go to next error (or msg)
  (local-set-key (kbd "M-[") 'previous-error)     ; Go to previous error or msg

  ;; Misc go stuff
  (auto-complete-mode 1))                         ; Enable auto-complete mode

;; Connect go-mode-hook with the function we just defined
(add-hook 'go-mode-hook 'my-go-mode-hook)

;; Ensure the go specific autocomplete is active in go-mode.
(with-eval-after-load 'go-mode
   (require 'go-autocomplete))

;; If the go-guru.el file is in the load path, this will load it.
(require 'go-guru)


(require 'go-eldoc)
(require 'go-autocomplete)
(require 'gotest)

;; Load modules for HTML, CSS and JS

(defun skip-to-next-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-forward " >")
    (unless (search-forward-regexp "^\\s *$" nil t)
      (goto-char (point-max)))))

(defun skip-to-previous-blank-line ()
  (interactive)
  (let ((inhibit-changing-match-data t))
    (skip-syntax-backward " >")
    (unless (search-backward-regexp "^\\s *$" nil t)
      (goto-char (point-min)))))

(eval-after-load "sgml-mode"
  '(progn
     (define-key html-mode-map
       [remap forward-paragraph] 'skip-to-next-blank-line)

     (define-key html-mode-map
       [remap backward-paragraph] 'skip-to-previous-blank-line)))


;; after deleting a tag, indent properly
(defadvice sgml-delete-tag (after reindent activate)
  (indent-region (point-min) (point-max)))

(require 'web-mode) ;; General web development

(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))


(require 'scss-mode) ;; CSS and SCSS
(require 'css-mode)

(require 'js2-mode)     ;; Javascript
(require 'js2-refactor)
(add-hook 'js2-mode-hook #'js2-refactor-mode)

(require 'typescript) ;; Typescript && Angular
(require 'tide)
(require 'ng2-mode)

(setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1))

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

(defun js-mode-bindings ()
    "Sets a hotkey for using the json-snatcher plugin"
      (when (string-match  "\\.json$" (buffer-name))
            (local-set-key (kbd "C-c C-g") 'jsons-print-path)))
(add-hook 'js-mode-hook 'js-mode-bindings)
(add-hook 'js2-mode-hook 'js-mode-bindings)

;; Keys Bindings
(require 'keys-bindings)
(require 'mode-mappings)
