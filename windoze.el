;;; XEmacs windoze setup -*- Mode:emacs-lisp -*-
;;; This file should work with XEmacs 20.x and greater

;; For Windoze, the HOME and DOMAINNAME environment variables *must* be set

(setq directory-sep-char ?/)

(defadvice call-process (around windowize activate)
  (let ((directory-sep-char ?\\))
    ad-do-it
    ))

;; For cmaxecp and others
(defadvice call-process-region (around windowize activate)
  (let ((directory-sep-char ?\\))
    ad-do-it
    ))

;; For compile
(defadvice start-process-shell-command (around windowize activate)
  (let ((directory-sep-char ?\\))
    ad-do-it
    ))

;; NT uses nmake and findstr
(set-variable 'grep-command "findstr -n ")
(setq compile-command "nmake ")
(setq make-clean-command "nmake clean all")
;; Make sure we get the right find. This was only a problem after reinstalling w2k
(setq igrep-find-program "e:/cygwin/bin/find")

(setq local-compile-command "cl /Ot /W3")

;; Also set in `vm-init.el'
(setq display-time-mail-file "c:/tmp/slurp.file")

;; SAM SAM SAM
;; For windoze only hashing seems to work with auto-save
(setq auto-save-directory (win32-short-file-name auto-save-directory))
(unless (emacs-version>= 21 4)
  (setq auto-save-hash-p t
	backup-hash-p t
	auto-save-hash-directory (concat auto-save-directory "hash/")))

(require 'backup-dir)
(setq bkup-backup-directory-info
      (cons (list 't (expand-file-name "~/backup") 'full-path 'prepend-name 'ok-create)
	    '()))

;; Strip down the error regexp for speed.
(setq compilation-error-regexp-systems-list (list 'msft))

;; SAM This binding is broken in later betas
(if (eq (key-binding [(shift insert)]) 'x-yank-clipboard-selection)
    (global-set-key [(shift insert)] 'mswindows-paste-clipboard))


;; For macro expansion
;; Note: This assumes cygwin installed
(when (would-like 'cmacexp)
  (setq c-macro-preprocessor "cpp -C"))

;; SAM FIX Does not work in NT
(remove-hook 'c-mode-common-hook 'my-compile-command)


;;--------------------------------------------------------------
(unless (emacs-version>= 21 4) ;; SAM
  ;; This is defined wrong in win32-native.el
  (defun make-auto-save-file-name (&optional file-name)
    "Return file name to use for auto-saves of current buffer.
Does not consider `auto-save-visited-file-name' as that variable is checked
before calling this function.  You can redefine this for customization.
See also `auto-save-file-name-p'."
    (let ((name (original-make-auto-save-file-name file-name))
	  (start 0))
      ;; destructively replace occurrences of * or ? with $
      (while (string-match "[?*]" name start)
	(aset name (match-beginning 0) ?$)
	(setq start (1+ (match-end 0))))
      name))
  )

;;--------------------------------------------------------------
;; Position the frames
(setq initial-frame-plist '(top 1 left 486 width 90 height 59)
      default-frame-plist '(top 1 left   1 width 90 height 59))
;;(setq initial-frame-plist '(top 1 left 480 width 80 height 48)
;;      default-frame-plist '(top 1 left   1 width 90 height 48))
;;(setq initial-frame-plist '(top 1 left 540 width 90 height 58)
;;      default-frame-plist '(top 1 left   1 width 90 height 58))

;; -----------------------------------------------------------
;; aspell for nt - will not work with ispell.el 3.3 or 3.4
(when (would-like 'ispell)
  (setq ispell-program-name "e:/aspell/aspell")
  ;;(setq ispell-extra-args '("--conf=d:/aspell/aspell.conf" "--dict-dir=d:/aspell/dict"))
  (when (string-match "3.0" ispell-version)
    (setq ispell-extra-args '("--reverse"))))


;; -----------------------------------------------------------
;; XEmacs NT does not downcase truenames (NTEmacs does).
;; This adds a handler that downcases the truenames.
(setq file-name-handler-alist
      (nconc file-name-handler-alist
	     '(("^[a-zA-Z]:" . downcase-filename-handler))))

(defun downcase-filename-handler (op &rest args)
  (if (eq op 'file-truename)
      ;; first argument is the filename
      (setcar args (downcase (car args))))
  ;; Now call the real handler with possibly changed args.
  ;; The following comes straight from the documentation in files.el
  (let ((inhibit-file-name-handlers
	 (cons 'downcase-filename-handler
	       (and (eq inhibit-file-name-operation op)
		    inhibit-file-name-handlers)))
	(inhibit-file-name-operation op))
    (apply op args)))


;; Setup ddk environment
(if (would-like 'ddk-setenv)
    (ddk-setenv))


;; From: "Mike Alexander" <mta@arbortext.com>
;; Date: Wed, 5 Sep 2001 00:22:50 -0400
;; If you have a value for width or height on default-frame-plist, you also
;; need to set default-msprinter-frame-plist with values for height and
;; width. Normally you would set them to nil:  to get the printer code to
;; calculate the appropriate size.
;; (setq default-msprinter-frame-plist height nil width nil)