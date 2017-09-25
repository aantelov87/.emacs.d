;;; phpfmt.el --- Interface to phpfmt command for php files

;; this is basically a copy of the necessary parts from
;; go-mode so all credit goes to
;; The Go Authors
;;
(defcustom phpfmt-command "phpfmt"
  "The 'phpfmt' command."
  :type 'string
  :group 'php)

(defun phpfmt ()
  "Format the current buffer according to the formatting tool.
The tool used can be set via ‘phpfmt-command` (default: phpfmt)"
  (interactive)
  (let ((tmpfile (make-temp-file "phpfmt" nil ".php"))
        (patchbuf (get-buffer-create "*phpfmt patch*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8)
	phpfmt-args)

     (unwind-protect
        (save-restriction
          (widen)
          (with-current-buffer patchbuf
            (erase-buffer))
          (write-region nil nil tmpfile)
          (setq phpfmt-args (append phpfmt-args
                                       (list "-o" tmpfile)))
          (message "Calling phpfmt: %s %s" phpfmt-command phpfmt-args)
          (if (zerop (apply #'call-process phpfmt-command nil nil nil phpfmt-args))
              (progn
                (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
                    (message "Buffer is already phpfmted")
                  (php--apply-rcs-patch patchbuf)
                  (message "Applied phpfmt"))
                )))

      (kill-buffer patchbuf)
      (delete-file tmpfile))))

(defun phpfmt-before-save ()
  "Add this to .emacs to run phpfmt on the current buffer before saving:
  (add-hook 'before-save-hook 'phpfmt-before-save)."
  (interactive)
  (when (eq major-mode 'php-mode) (phpfmt)))


(defun php--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current
buffer."
  (let ((target-buffer (current-buffer))
        ;; Relative offset between buffer line numbers and line numbers
        ;; in patch.
        ;;
        ;; Line numbers in the patch are based on the source file, so
        ;; we have to keep an offset when making changes to the
        ;; buffer.
        ;;
        ;; Appending lines decrements the offset (possibly making it
        ;; negative), deleting lines increments it. This order
        ;; simplifies the forward-line invocations.
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error "invalid rcs patch or internal error in php--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond
             ((equal action "a")
              (let ((start (point)))
                (forward-line len)
                (let ((text (buffer-substring start (point))))
                  (with-current-buffer target-buffer
                    (decf line-offset len)
                    (goto-char (point-min))
                    (forward-line (- from len line-offset))
                    (insert text)))))
             ((equal action "d")
              (with-current-buffer target-buffer
                (php--goto-line (- from line-offset))
                (incf line-offset len)
                (php--delete-whole-line len)))
             (t
              (error "invalid rcs patch or internal error in php--apply-rcs-patch")))))))))

(defun php--goto-line (line)
  (goto-char (point-min))
  (forward-line (1- line)))

(defun php--delete-whole-line (&optional arg)
  "Delete the current line without putting it in the `kill-ring'.
Derived from function `kill-whole-line'.  ARG is defined as for that
function."
  (setq arg (or arg 1))
  (if (and (> arg 0)
           (eobp)
           (save-excursion (forward-visible-line 0) (eobp)))
      (signal 'end-of-buffer nil))
  (if (and (< arg 0)
           (bobp)
           (save-excursion (end-of-visible-line) (bobp)))
      (signal 'beginning-of-buffer nil))
  (cond ((zerop arg)
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (end-of-visible-line) (point))))
        ((< arg 0)
         (delete-region (progn (end-of-visible-line) (point))
                        (progn (forward-visible-line (1+ arg))
                               (unless (bobp)
                                 (backward-char))
                               (point))))
        (t
         (delete-region (progn (forward-visible-line 0) (point))
                        (progn (forward-visible-line arg) (point))))))

(provide 'phpfmt)

;;; phpfmt.el ends here
