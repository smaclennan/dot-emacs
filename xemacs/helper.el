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

(when window-system
  ;; Pointer used during garbage collection.
  ;; .xbm not supported under windoze
  (let ((dir (concat dot-dir "xemacs/etc/")) img mask)
    (if (string= (x-server-vendor) "Colin Harrison")
	;; xming only supports 32x32
	(setq img  (concat dir "recycle-image-32.xbm")
	      mask (concat dir "recycle-mask-32.xbm"))
      (setq img  (concat dir "recycle-image.xbm")
	    mask (concat dir "recycle-mask.xbm")))
    (if (and img mask (not running-windoze))
	(set-glyph-image gc-pointer-glyph
			 (vector 'xbm
				 :file img
				 :mask-file mask
				 :foreground "black"
				 :background "chartreuse1"))
      (set-glyph-image gc-pointer-glyph "recycle2.xpm"))))

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

(setq package-get-remote
      ;;'("ftp.ca.xemacs.org" "/pub/Mirror/xemacs/beta/experimental/packages")))
      '("ftp.xemacs.org" "/pub/xemacs/xemacs-21.5/experimental/packages"))

(defun server-start ()
  (gnuserv-start)
  (setq gnuserv-frame (selected-frame)))

(defun xref-find-definitions (find-tag))
