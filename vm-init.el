;; VM setup -*- emacs-lisp -*-

; To use:

; ;; Move the .vm init file to .xemacs
; (setq vm-init-file (concat dot-dir "vm-init.el")
;      vm-folders-summary-database (concat dot-dir ".vm.folders.db"))
; (require 'vm)

; I am down to one environment that I must handle:
;    fetchmail/procmail for incoming mail
;    smtpmail for outgoing mail

; Assumptions:
;  smtpmail already setup in init.el
;  user-mail-address is initialized

;; Make sure these are right up front!
(setq vm-folder-directory (expand-file-name "~/Mail/"))
(setq vm-primary-inbox (concat vm-folder-directory "INBOX")
      vm-crash-box (concat vm-folder-directory "CRASHBOX"))

;; So mail-mode-map will exist
(require 'sendmail)

;; --- Simple initialization
(setq-default vm-summary-show-threads t)

(setq vm-move-after-deleting	t
      vm-move-after-killing	t
      vm-preview-lines		nil
      vm-reply-subject-prefix	"Re: "
      vm-delete-after-saving	t
      mail-signature		nil)

;; I like to know what mailer people are using...
(append-to-list 'vm-visible-headers "X-Mailer:")
(append-to-list 'vm-visible-headers "User-Agent:")

;; Spamassassin adds this, lets display it
(append-to-list 'vm-visible-headers "X-Spam-Level:")

;; Expunge all mail on exit
(add-hook 'vm-quit-hook 'vm-expunge-folder)

;; This puts a nice flag on my messages in Outlook
(setq mail-default-headers
      "X-Message-Flag: Warning: This message may contain actual content!\n")

;; This puts an exclamation mark in Outlook
(defun vm-flag-important ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward mail-header-separator nil t)
	(progn
	  (goto-char (match-beginning 0))
	  (insert "X-Priority: 1\n"))
      (warning "Cannot find mail header separator"))))
(define-key mail-mode-map [(control c) !] 'vm-flag-important)

;;(defvar vm-bmf-program "/usr/local/bin/vmstrip | /usr/local/bin/bmf -S")
(defvar vm-bmf-program "/usr/local/bin/vmstrip | /usr/bin/hose zot 8080 --netslave")
(defun vm-flag-spam ()
  (interactive)
  (vm-pipe-message-to-command vm-bmf-program '(256))
  (vm-delete-message 1))
(define-key vm-mode-map ?' 'vm-flag-spam)

;; Do not reply to Cc's
(fset 'vm-toolbar-reply-command 'vm-reply-include-text)

;; This is getting ridiculous, I can't keep up with all the
;; charsets (Outlook seems to mutate them daily.)
;; Lets default to all and see what happens
(setq vm-mime-default-face-charsets t)

;; XEmacs >= 21.2 has a module for base64
(when nil ;; SAM HACK
(if (fboundp 'load-module)
    (if (emacs-version>= 21 2 45)
	(load-module "base64/base64")
      (load-module "base64"))
  ;; Use the external programs if possible
  (setq vm-mime-base64-encoder-program (exec-installed-p "base64-encode")
	vm-mime-base64-decoder-program (exec-installed-p "base64-decode")))
)

(unless running-windoze
  (setq vm-mime-external-content-types-alist
	'(("image"		"xv")
	  ("audio/x-pn-realaudio" "rvplayer")
	  ("video"		"xanim")
	  ("application/wordperfect5.1" "xwp")
	  ("pdf"			"xpdf")))
  )


;; Images are not working properly
(setq vm-mime-internal-content-type-exceptions '("text/html"
						 "image/jpeg"
						 "image/gif"
						 ))

;; Setup some virtual folders
(setq vm-virtual-folder-alist
      (list
       (list "new"
	     (list (list vm-primary-inbox)
		   (list 'new)))
       (list "xemacs"
	     (list (list vm-primary-inbox)
		   (list 'header "Sender:.*@xemacs.org")))
       ;; Yes, I actually use this
       (list "not xemacs"
	     (list (list vm-primary-inbox)
		   (list 'not (list 'header "Sender:.*@xemacs.org"))))
       (list "meetings"
	     (list (list vm-primary-inbox)
		   (list 'text (regexp-quote "*~*~*~*~*~*~*~*~*~*"))))
       (list "defects"
	     (list (list vm-primary-inbox)
		   (list 'subject "Defect [0-9]+ has been assigned to you")))
       ))

(setq vm-auto-folder-alist
      '(("Sender"
	 ("xemacs-beta.*@xemacs.org" . "xemacs-beta"))
	("Sender"
	 ("xemacs@xemacs.org" . "xemacs"))
	("Sender"
	 ("xemacs-design.*@xemacs.org" . "xemacs-design"))
	("Subject"
	 ("FTE Small" . "Ranger"))
	("Resent-From"
	 ("redhat-announce-list@redhat.com" . "redhat"))
	("To"
	 ("0xdeadbeef" . "deadbeef"))
	("Resent-From"
	 ("^coda" . "coda"))
	("Resent-From"
	 ("clustering@alinka.com" . "clustering"))
	("Subject"
	 ("YOW PPP" . "yow-ppp"))
	))

;; --- Summary line
;; run (vm-fix-my-summary!!!) after changing this
;; This line has sizes
(setq vm-summary-format "%3.3m %2d %*%a%UA %-17.17F %4UK %I\"%s\"\n")
;; This line has none
;;(setq vm-summary-format "%3.3m %2d %*%a%UA %-17.17F %I%s\n")

(defun vm-summary-function-A (msg)
  "Add an `A' for attachments (but exclude vcards)"
  ;; vm does a save-excursion for us!
  (goto-char (point-min))
  (if (search-forward "Content-Disposition: attachment" nil t)
      (if (search-forward "Content-Disposition: attachment" nil t)
	  "A"
	(goto-char (point-min))
	(if (search-forward "Content-Type: text/x-vcard" nil t)
	    " "
	  "A"))
    " "))

;; This is just `vm-su-byte-count' with a / 1024
(defun vm-summary-function-K (m)
  "Convert char count to k."
  (let ((bytes (vm-su-byte-count m)))
    (int-to-string (/ (string-to-int bytes) 1024))))

(defun tidy (header match)
  (save-excursion
    (goto-char (point-min))
    (when (search-forward (concat header " " match) nil t)
      (let ((current (vm-get-header-contents
		      (vm-real-message-of (car vm-message-pointer))
		      "Message-ID:"))
	    m from delete-list found)
	;; Find all older messages
	(catch 'found
	  (dolist (msg vm-message-list)
	    (setq m (vm-real-message-of msg))
	    (setq from (vm-get-header-contents m header))
	    (when (and from (string-match match from))
	      (if (string= current (vm-get-header-contents m "Message-ID:"))
		  (throw 'found t)
		(push msg delete-list))))
	  ;; Paranoia - no match for current message
	  (error "Unable to match current message!"))
	;; Delete old messages
	(dolist (msg delete-list)
	  (vm-set-deleted-flag msg t)
	  (message "delete %s" (vm-get-header-contents
			 (vm-real-message-of msg) "Subject:"))
      )))))

(defun tidy-up ()
  ;; When we receive a new e-Weather delete all other e-Weathers
  (tidy "From:"
	"\"WeatherOffice\" <WeatherOffice@ec.gc.ca>")
  ;; Same for fetchmail notices
  (tidy "Subject:"
	"The [0-9.]+ release of fetchmail is available"))
(add-hook 'vm-select-new-message-hook 'tidy-up)

;; --- BBDB
(when (would-like 'bbdb)
  ;; The BBDB should be seen but not heard.
  (setq bbdb-always-add-addresses	t
	bbdb-new-nets-always-primary	t
	bbdb-offer-save		'justdoit
	bbdb-use-pop-up		nil)

  ;; C-c c is way to close to C-c C-c
  (define-key mail-mode-map [(control c) b] 'bbdb-complete-name)

  ;; Keep it in the mail folder
  (setq bbdb-file (concat vm-folder-directory "bbdb"))

  (bbdb-initialize 'vm)

  ;; Save the .bbdb file automagically before exiting
  (defun my-save-bbdb ()
    (save-excursion
      (condition-case ()
	  (progn
	    (set-buffer (file-name-nondirectory bbdb-file))
	    (save-buffer))
	(error nil))))
  (add-hook 'vm-quit-hook 'my-save-bbdb))

;; --- Smilies from gnus
(when (packagep 'gnus)
  (custom-set-variables
   '(smiley-regexp-alist (quote smiley-nosey-regexp-alist)))
  (add-hook 'vm-select-message-hook 'smiley-buffer t))


;; --- Outgoing mail
(setq mail-archive-file-name
      (expand-file-name "Outgoing" vm-folder-directory))
(unless t ;;running-windoze
  (unless (file-exists-p "~/bin/mv-outgoing")
    (warn (concat "~/bin/mv-outgoing does not exist. See more...\n"
		   "0 0 1 * * $HOME/bin/mv-outgoing "
		   mail-archive-file-name))))

;; --- spell check message body
;; NOTE: This overrides `vm-iconify-frame'
(define-key vm-mode-map "i" 'ispell-message) ;; really C-c C-v i

;; --- Mail crypt
(when (would-like 'mailcrypt)
  (mc-setversion 'gpg)

  (autoload 'mc-install-write-mode "mailcrypt" nil t)
  (autoload 'mc-install-read-mode "mailcrypt" nil t)
  (add-hook 'mail-mode-hook 'mc-install-write-mode)

  ;; VM specific
  (add-hook 'vm-mode-hook 'mc-install-read-mode)
  (add-hook 'vm-summary-mode-hook 'mc-install-read-mode)
  (add-hook 'vm-virtual-mode-hook 'mc-install-read-mode)
  (add-hook 'vm-mail-mode-hook 'mc-install-write-mode))

(when (would-like 'vm-opera)
  (require 'vm-menu)
  (vm-opera-init))

(cond
 ((exec-installed-p "firefox")
  (setq vm-url-browser 'vm-mouse-send-url-to-mozilla))
 ((exec-installed-p "mozilla")
  (setq vm-url-browser 'vm-mouse-send-url-to-mozilla))
 ((exec-installed-p "netscape")
  (setq vm-url-browser 'vm-mouse-send-url-to-netscape))
 )
