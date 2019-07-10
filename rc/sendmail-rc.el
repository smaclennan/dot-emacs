;; I use doorknob just about everywhere... so default to sendmail
(when (eq send-mail-function 'sendmail-query-once)
  (setq send-mail-function 'sendmail-send-it))
