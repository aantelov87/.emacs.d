(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

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
  (web-mode-use-tabs)

  (setq-local coffee-tab-width n) ; coffeescript
  (setq-local javascript-indent-level n) ; javascript-mode
  (setq-local js-indent-level n) ; js-mode
  (setq-local js2-basic-offset n) ; js2-mode, in latest js2-mode, it's alias of js-indent-level
  (setq-local web-mode-markup-indent-offset n) ; web-mode, html tag in html file
  (setq-local web-mode-css-indent-offset n) ; web-mode, css in html file
  (setq-local web-mode-code-indent-offset n) ; web-mode, js code in html file
  (setq-local css-indent-offset n) ; css-mode

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

(add-hook 'coding-hook 'local-column-number-mode)
(add-hook 'coding-hook 'local-comment-auto-fill)
(add-hook 'coding-hook 'turn-on-save-place-mode)
(add-hook 'coding-hook 'add-watchwords)
(add-hook 'coding-hook 'turn-on-whitespace)
(add-hook 'coding-hook 'run-indent)


(defun run-coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (run-hooks 'coding-hook))

(add-hook 'php-mode-hook 'run-coding-hook)
(add-hook 'c-mode-hook 'run-coding-hook)
(add-hook 'scss-mode-hook 'run-coding-hook)
(add-hook 'css-mode-hook 'run-coding-hook)
(add-hook 'go-mode-hook 'run-coding-hook)
(add-hook 'web-mode-hook 'run-coding-hook)
(add-hook 'typescript-mode-hook 'run-coding-hook)
(add-hook 'js-mode-hook 'run-coding-hook)
(add-hook 'js2-mode-hook 'run-coding-hook)
(add-hook 'sgml-mode-hook 'run-coding-hook)
(add-hook 'clojure-mode-hook 'run-coding-hook)
; (eval-after-load 'scheme-mode '(add-hook 'scheme-mode-hook 'run-coding-hook))
;; (eval-after-load 'ruby-mode '(add-hook 'ruby-mode-hook 'run-coding-hook))

(provide 'coding-hook)
