(defun sh-to-ksh (entry)
  (when (eq (cdr entry) 'sh-mode)
    (setcdr entry 'ksh-mode))
  entry)

;; Convert sh-mode to ksh-mode
(mapc 'sh-to-ksh auto-mode-alist)
(mapc 'sh-to-ksh interpreter-mode-alist)

(setq ksh-indent 4)

;; Bold SAM comments
(comment-warn (list ksh-font-lock-keywords) 'ksh-mode)
