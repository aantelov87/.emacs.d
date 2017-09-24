(require 'thingatpt)
(require 'imenu)

;; Network
(defun view-url ()
	"Open a new buffer containing the contents of URL."
	(interactive)
	(let* ((default (thing-at-point-url-at-point))
	 (url (read-from-minibuffer "URL: " default)))
		(switch-to-buffer (url-retrieve-synchronously url))
		(rename-buffer url t)
		;; TODO: switch to nxml/nxhtml mode
		(cond ((search-forward "<?xml" nil t) (xml-mode))
		((search-forward "<html" nil t) (html-mode)))))

(defun cleanup-buffer ()
	"Perform a bunch of operations on the whitespace content of a buffer."
	(interactive)
	(whitespace-cleanup)
	(indent-buffer))

(eval-after-load 'paredit
	'(add-to-list 'paredit-space-for-delimiter-predicates
		'esk-space-for-delimiter?))

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

;; Functions for php
(defun my-php-mode-hook ()
	(auto-complete-mode t)
	(require 'ac-php)
	(setq ac-sources	'(ac-source-php ) )
	(yas-global-mode 1))

;; Funcs for golang
(defun go-mode-setup ()
	(setq compile-command "go vet && errcheck && go test -v && go build -v")
	(define-key (current-local-map) "\C-c\C-c" 'compile)
	(go-eldoc-setup)
	(setq gofmt-command "goimports")
	(add-hook 'before-save-hook 'gofmt-before-save)

	;; guru settings
	(go-guru-hl-identifier-mode)				; highlight identifiers

	;; Key bindings specific to go-mode
	(local-set-key (kbd "M-.") 'godef-jump)		; Go to definition
	(local-set-key (kbd "M-*") 'pop-tag-mark)		; Return from whence you came
	(local-set-key (kbd "M-p") 'compile)			; Invoke compiler
	(local-set-key (kbd "M-P") 'recompile)		; Redo most recent compile cmd
	(local-set-key (kbd "M-]") 'next-error)		; Go to next error (or msg)
	(local-set-key (kbd "M-[") 'previous-error)		; Go to previous error or msg

	;; Misc go stuff
	(auto-complete-mode 1))

;; Func for HTML, CSS, JS
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

(defun js-mode-bindings ()
	"Sets a hotkey for using the json-snatcher plugin"
	(when (string-match	 "\\.json$" (buffer-name))
		(local-set-key (kbd "C-c C-g") 'jsons-print-path)))


;; Func for typescript mode
(defun setup-tide-mode ()
	(interactive)
	(tide-setup)
	(flycheck-mode +1)
	(setq flycheck-check-syntax-automatically '(save mode-enabled))
	(eldoc-mode +1)
	(tide-hl-identifier-mode +1))

(defun extension-tide-mode ()
	(when (string-equal "tsx" (file-name-extension buffer-file-name))
		(setup-tide-mode)))

;; after deleting a tag, indent properly
(defadvice sgml-delete-tag (after reindent activate)
	(indent-region (point-min) (point-max)))

(defadvice align-regexp (around align-regexp-with-spaces activate)
	 (let ((indent-tabs-mode nil))
		 ad-do-it))

;;; These belong in coding-hook:

;; We have a number of turn-on-* functions since it's advised that lambda
;; functions not go in hooks. Repeatedly evaling an add-to-list with a
;; hook value will repeatedly add it since there's no way to ensure
;; that a lambda doesn't already exist in the list.
(defun run-indent()
  (setup-indent 8))

(defun setup-indent (n)
  ;; Turn on tabs
  (setq indent-tabs-mode t)
  (setq-default indent-tabs-mode t)
  ;; make tab key always call a indent command.
  (setq-default tab-always-indent t)
  ;; make tab key do indent first then completion.
  ;;	(setq-default tab-always-indent 'complete)
  ;; Set the tab width
  (setq default-tab-width n)
  (setq tab-width n)
  (setq c-basic-indent n)

  ;; java/c/c++
  (setq-local c-basic-offset n)
  ;; web development
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)


  (setq-local coffee-tab-width n) ; coffeescript
  (setq-local javascript-indent-level n) ; javascript-mode
  (setq-local js-indent-level n) ; js-mode
  (setq-local js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n) ; css-mode
  (web-mode-use-tabs)
  )

(defun local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun turn-on-save-place-mode ()
  (setq save-place t))

(defun turn-on-whitespace ()
  (whitespace-mode t))

(defun turn-on-paredit ()
  (paredit-mode t))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
	  1 font-lock-warning-face t))))

(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (add-hook 'before-save-hook 'cleanup-buffer)
  (run-hooks 'coding-hook))


(provide 'defuns)
;;; defuns.el ends here
