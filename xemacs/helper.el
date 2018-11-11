;; Build helper - plus some extras much like esp.el

(defvar dot-dir (expand-file-name "~/.xemacs/"))

(dolist (dir '("lisp" "misc" "xemacs"))
  (add-to-list 'load-path (concat dot-dir dir)))
(add-to-list 'data-directory-list (concat dot-dir "etc") t)
(add-to-list 'data-directory-list (concat dot-dir "xemacs/etc") t)

;; For rcfiles to be able to match loaded lisp such as lisp-mode we
;; need to turn the file names into simple load names.
(setq load-history
      (mapcar (lambda (elt)
		(let ((a (car elt)))
		  (if (eq 0 (string-match "/.*/\\([^/]+\\)\.elc" a))
		      (list (match-string 1 a))
		    (list a))))
	      load-history))

;; XEmacs 21.5 cruft
(when (emacs-version>= 21 5)
  ;; For some reason the file coding was gutted - put it back
  (setq buffer-file-coding-system-for-read 'undecided
	default-buffer-file-coding-system  'raw-text))

(defadvice abbreviate-file-name (before add-hack-homedir activate)
  "Always set HACK-HOMEDIR to t ala GNU Emacs."
  (ad-set-arg 1 t))

;; -------------------------------------------------------------------------
;; KSH MODE

;; sh-mode doesn't work too well in XEmacs. It doesn't handle case
;; labels properly. GNU Emacs handles case labels, but doesn't indent
;; comments properly. ksh-mode seems to handle case labels and
;; comments, so let's switch to that if it is available.
;;
;; ksh-mode not avaliable in Emacs, and turning it on loses font-lock
;; and bracket matching... so enable it only for xemacs for now

(defun sh-to-ksh (entry)
  (when (eq (cdr entry) 'sh-mode)
    (setcdr entry 'ksh-mode))
  entry)

;; Convert sh-mode to ksh-mode
(mapc 'sh-to-ksh auto-mode-alist)
(mapc 'sh-to-ksh interpreter-mode-alist)
