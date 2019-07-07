;; Very experimental

;; This file is called before .gnus

;; .authinfo or .authinfo.gpg
;; machine imap.gmail.com login <USER> password <APP-PASSWORD> port imaps
;; machine smtp.gmail.com login <USER> password <APP-PASSWORD> port 587

;; Should probably be in ~/.gnus
(if nil
    (setq gnus-select-method '(nnimap "imap.gmail.com"))
  (setq gnus-select-method
	'(nnimap "gmail"
		 (nnimap-address "imap.gmail.com")
		 (nnimap-server-port "imaps")
		 (nnimap-stream ssl)
		 ))
  )

;; SAM should this be in mail?
;;(setq send-mail-function 'smtpmail-send-it)

;;(setq smtpmail-smtp-server "smtp.gmail.com"
;;      smtpmail-smtp-service 587)

;; SAM what is this for?
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\"]\"[#'()]")

;; The .newsrc-dribble file doesn't seem to be useful for mail.
(setq gnus-use-dribble-file nil)

(defun my-update-gnus-summary ()
  (cond
   ((eq major-mode 'gnus-summary-mode)
    (gnus-summary-insert-new-articles))
   ((eq major-mode 'gnus-article-mode)
    (dolist (buff (buffer-list))
      (when (eq (buffer-local-value 'major-mode buff) 'gnus-summary-mode)
	(with-current-buffer buff
	  (gnus-summary-insert-new-articles)))))))

(defvar my-gnus-timer (run-at-time t 60 'my-update-gnus-summary))

;; Should probably be in ~/.gnus? Or at least make the trash a variable
(defun my-gnus-imap-delete ()
  "Delete the current message by moving it to the Trash."
  (interactive)
  (gnus-summary-move-article nil "[Gmail]/Trash")
  ;; The move is considered a "cancel"
  (gnus-summary-limit-to-marks (list gnus-canceled-mark) 'reverse)
  )

(define-key gnus-summary-mode-map [delete] 'my-gnus-imap-delete)
