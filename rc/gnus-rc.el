;; Very experimental

;; This file is called before .gnus

;; .authinfo or .authinfo.gpg
;; machine imap.gmail.com login <USER> password <APP-PASSWORD> port 993
;; machine smtp.gmail.com login <USER> password <APP-PASSWORD> port 587

;; .gnus.el
;; (setq gnus-select-method '(nnimap "gmail"
;; 				  (nnimap-address "imap.gmail.com")
;; 				  (nnimap-server-port "993")
;; 				  (nnimap-stream ssl)))
;;
;; (setq gnus-message-archive-method '(nnimap "imap.gmail.com")
;;       gnus-message-archive-group "[Gmail]/Sent")

;; .gnus.el or sendmail-rc.el
;; (setq send-mail-function 'smtpmail-send-it)
;; (setq smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

(when (package-installed-p 'bbdb)
  (bbdb-initialize 'gnus 'message))

;; The .newsrc-dribble file doesn't seem to be useful for mail.
(setq gnus-use-dribble-file nil)

(defun my-update-gnus-summary ()
  "Check to see if a gnus group or summary window exists"
  (dolist (buff (mapcar (lambda (w) (window-buffer w)) (window-list)))
    (let ((mode (buffer-local-value 'major-mode buff)))
      (cond ((eq mode 'gnus-summary-mode)
	     (with-current-buffer buff
	       (gnus-summary-insert-new-articles)))
	    ((eq mode 'gnus-group-mode)
	     (with-current-buffer buff
	       (gnus-group-get-new-news)))))))

(defvar my-gnus-timer (run-at-time t 60 'my-update-gnus-summary))

(defvar my-gnus-trash-folder "[Gmail]/Trash"
  "The Trash folder.")

(defun my-gnus-imap-delete ()
  "Delete the current message by moving it to the Trash."
  (interactive)
  (gnus-summary-move-article nil my-gnus-trash-folder)
  ;; The move is considered a "cancel"
  (gnus-summary-limit-to-marks (list gnus-canceled-mark) 'reverse)
  )

;; Seems this is too early to set keys, so do it in a hook.
(add-hook
 'gnus-summary-mode-hook
 (lambda ()
   (define-key gnus-summary-mode-map [delete] 'my-gnus-imap-delete)
   (define-key gnus-summary-mode-map [deletechar] 'my-gnus-imap-delete)))
