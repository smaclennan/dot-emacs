;; Build helper - plus some extras much like esp.el

(defvar dot-dir (expand-file-name "~/.xemacs/"))

(dolist (dir '("lisp" "misc" "xemacs"))
  (add-to-list 'load-path (concat dot-dir dir)))
(add-to-list 'data-directory-list (concat dot-dir "etc") t)
(add-to-list 'data-directory-list (concat dot-dir "xemacs/etc") t)

(dolist (dir '("lisp" "misc" "xemacs"))
  (load (concat dot-dir dir "/auto-autoloads.el") t))

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

;; Filladapt is a syntax-highlighting package.  When it is enabled it
;; makes filling (e.g. using M-q) much much smarter about paragraphs
;; that are indented and/or are set off with semicolons, dashes, etc.
(defun add-filladapt()
  (require 'filladapt) ;; No autoloads
  (turn-on-filladapt-mode))
(add-hook 'text-mode-hook 'add-filladapt)
(add-hook 'mail-mode-hook 'add-filladapt)

;; C-h =
(define-key help-map ?= #'introspect-cursor-position)

;; Handy functions that where hard to work out
(defun my-get-face-foreground (face)
  (cdr (specifier-specs (face-foreground face) 'global)))
(defun my-get-face-background (face)
  (cdr (specifier-specs (face-background face) 'global)))
;; (my-get-face-background 'default)

(defun hack-modeline-background ()
  (let ((bg (face-background-instance 'modeline)))
    (when (color-instance-p bg)
      (set-face-background 'modeline bg))))
(when window-system
  (add-hook 'after-init-hook 'hack-modeline-background))

;; -------------------
;; Laptop Mode Helper
(defun check-faces ()
  (interactive)
  (let (found-one
	(font (if (listp laptop-mode-font) (nth 1 laptop-mode-font) laptop-mode-font)))
    (dolist (face (face-list))
      (unless (equal (face-font-name face) font)
	(setq found-one t)
	(message "%S %s" face (face-font-name face))))
    (if found-one
	(message "Check the message log")
      (message "OK"))))

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

(setq package-get-remote
      ;;'("ftp.ca.xemacs.org" "/pub/Mirror/xemacs/beta/experimental/packages")))
      '("ftp.xemacs.org" "/pub/xemacs/xemacs-21.5/experimental/packages"))
