(defvar coding-hook nil
  "Hook that gets run on activation of any programming mode.")

(add-hook 'coding-hook 'local-column-number-mode)
(add-hook 'coding-hook 'local-comment-auto-fill)
(add-hook 'coding-hook 'turn-on-save-place-mode)
(add-hook 'coding-hook 'add-watchwords)
(add-hook 'coding-hook 'turn-on-whitespace)
(add-hook 'coding-hook 'run-indent)

(add-hook 'php-mode-hook 'run-coding-hook)
(add-hook 'scss-mode-hook 'run-coding-hook)
(add-hook 'css-mode-hook 'run-coding-hook)
(add-hook 'go-mode-hook 'run-coding-hook)
(add-hook 'web-mode-hook 'run-coding-hook)
(add-hook 'js-mode-hook 'run-coding-hook)
(add-hook 'js2-mode-hook 'run-coding-hook)
(add-hook 'sgml-mode-hook 'run-coding-hook)
(add-hook 'clojure-mode-hook 'run-coding-hook)
(add-hook 'protobuf-mode-hook 'run-coding-hook)

(provide 'coding-hook)
