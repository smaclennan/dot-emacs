;; Very experimental - I never really got it working.
;; Most of this should be in .gnus.el...

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
  (bbdb-initialize 'gnus))

;; The .newsrc-dribble file doesn't seem to be useful for mail.
(setq gnus-use-dribble-file nil)

(setq gnus-agent nil)

(when nil ;; SAM try gnu-demon
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

;; Crank this down for debugging
;;(setq gnus-demon-timestep 10)

;; SAM (gnus-demon-add-handler 'my-update-gnus-summary 1 nil)

;;(gnus-demon-remove-handler 'my-update-gnus-summary)
)

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

;; gnus-summary-insert-new-articles

;; Does get new emails.
;; Does not update the other email clients.

;; M-g gnus-summary-rescan-group

;; Works but reads one of the unread mail. And at the last mail it
;; shows all the old stuff.
;; Also closes open connection?

;; I believe that gnus cannot handle an empty summary. It seems to
;; update to the old stuff after my-gnus-summary-rescan-group now.


;; This does fix the "reads one email"
((defun my-gnus-summary-rescan-group (&optional all)
  "Exit the newsgroup, ask for new articles, and select the newsgroup."
  (interactive "P")
  (let ((config gnus-current-window-configuration))
    (gnus-summary-reselect-current-group all t)
    (gnus-configure-windows config)
    ;; (when (eq config 'article)
    ;;  (gnus-summary-select-article))
    ))

;; SAM needed?
(defun my-gnus-summary-read-group (orig-fun group &optional show-all &rest args)
  "Force SHOW-ALL to nil."
  (apply orig-fun group nil args))

(advice-add 'gnus-summary-read-group :around #'my-gnus-summary-read-group)

;; SAM needed?
(defun my-gnus-summary-reselect-current-group (orig-fun &optional all &rest args)
  "Force ALL to nil."
  (apply orig-fun nil args))

(advice-add 'gnus-summary-reselect-current-group :around #'my-gnus-summary-reselect-current-group)
