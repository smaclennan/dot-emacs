;; I use doorknob everywhere... so default to sendmail
(when (eq send-mail-function 'sendmail-query-once)
  (setq send-mail-function 'sendmail-send-it))

;; For some reason Emacs does not add the date
(defun mail-add-date ()
  (save-excursion
    (beginning-of-line)
    (insert (format-time-string "Date: %a, %d %b %Y %T %z\n"))))
(add-hook 'mail-setup-hook 'mail-add-date)
