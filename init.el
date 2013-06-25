;; S?X?Emacs setup -*- Mode:emacs-lisp -*-
;; This file should work with XEmacs 2x.x, Emacs 21.x, or SXEmacs

;; Assumes at least the following packages:
;;	xemacs-base, edit-utils, cc-mode, ediff, pc

;;{{{ Configuration variables / functions

(defvar running-windoze (eq system-type 'windows-nt)
  "Non-nil if running Windows.")

(defvar running-as-root (string= (user-login-name) "root")
  "Non-nil if running as root.")

(defvar dot-dir
  ;; When called from load
  (if load-file-name
      (file-name-directory load-file-name)
    (if user-init-file
	(file-name-directory user-init-file)
      ;; for batch mode
      (let ((dir (pwd)))
	(when (string-match "^Directory " dir)
	  (replace-match "" nil nil dir)))))
  "The init file directory.")

;; We need to setup the load-path before we can require sam-common
;; Not xemacs is safer for old GNU Emacs versions (see sam-common)
(when (not (featurep 'xemacs))
  (require 'cl)
  (add-to-list 'load-path (concat dot-dir "esp"))
  ;; Add the local site-packages
  (let ((lisp-dir (concat dot-dir "site-packages/lisp")))
    (loop for dir in (directory-files lisp-dir t "^[^.i]") do
      (add-to-list 'load-path dir)))
  (load "esp-loaddefs" t t)
  (load "sam-loaddefs" t t)
  (load "missing" t t))

(require 'sam-common)

;; With the new package system, there is a greater chance a
;; package may be missing. Instead of an error, just add the
;; package to a list of missing packages and move on.
;; Note: returns non-nil if package exists.

(defvar would-have-liked-list nil
  "List of features that `would-like' and `packagep' could not find.")

(defun would-like (feature &optional no-list)
  "A less strident `require'."
  (condition-case nil
      (require feature)
    (error
     (unless no-list (add-to-list 'would-have-liked-list feature))
     nil)))

(defun packagep (package &optional no-list)
  (my-feature-cond
   (xemacs (if (assq package packages-package-list)
	       t
	     (unless no-list (add-to-list 'would-have-liked-list package))
	     nil))
   (emacs (would-like package no-list))))

(if (would-like 'rcfiles)
    (rcfiles-register-rc-files)
  (setq rcfiles-directory (concat dot-dir "rc")))
(add-to-list 'load-path rcfiles-directory)

;;}}}

;;{{{ Basic Customization

(setq debug-on-error t
      track-eol t
      kill-whole-line t
      next-line-add-newlines nil
      delete-key-deletes-forward t
      find-file-compare-truenames t
      signal-error-on-buffer-boundary nil
      inhibit-default-init t
      inhibit-startup-message t)

(setq visible-bell t)

(setq custom-file (concat dot-dir "custom.el"))
(load custom-file t)

(put 'narrow-to-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(when (not (emacs-version>= 21 2))
  (setq temp-buffer-shrink-to-fit t))

(would-like 'redo (featurep 'emacs)) ;; edit-utils

;; (would-like 'uncompress) ;; os-utils
(and (not noninteractive)
     (would-like 'jka-compr)
     (auto-compression-mode 1))

;; Turn off some modes/functions if they are missing

;; Always turn this mode off
(fset 'xrdb-mode 'ignore)

(my-feature-cond
 (xemacs (paren-set-mode 'paren t))
 (t (show-paren-mode t)))

;; This is defined in apel - here is a simple version
(unless (fboundp 'exec-installed-p)
  (defun exec-installed-p (file)
    "Return absolute-path of FILE if FILE is executable."
    (and running-windoze
	 (not (file-name-extension file))
	 (setq file (concat file ".exe")))
    (if (file-exists-p file)
	t
      (catch 'found
	(dolist (path exec-path)
	  (when (string-match "/+$" path)
	    (setq path (replace-match "" nil nil path)))
	  (setq path (concat path "/" file))
	  (when (file-exists-p path)
	    (throw 'found path)))))))

;; Split the system-name up into host and domain name.
(defvar host-name nil)
(defvar domain-name nil)
(let ((system-name (system-name)))
  (if (string-match "^\\([^.]+\\)\\.\\(.*\\)" system-name)
      ;; fully qualified system-name
      (setq host-name (match-string 1 system-name)
	    domain-name (match-string 2 system-name))
  ;; system-name is host-name
  (setq host-name system-name
	domain-name (getenv "DOMAINNAME"))))

;; These are missing
(unless (boundp 'lpr-command)
  (setq lpr-command "lpr"
	lpr-switches nil))

;; cl-loop required for packages like etags under SXEmacs, but require does
;; not seem to work in 22.1.9. So explicitly load the module.
(my-feature-cond (sxemacs (load-module "cl-loop")))

;;}}}

;;{{{ XEmacs 21.5 stuff

(my-feature-cond
 (xemacs
  (when (emacs-version>= 21 5)
    ;; For some reason the file coding was gutted - put it back
    (setq buffer-file-coding-system-for-read 'undecided
	  default-buffer-file-coding-system  'raw-text)

    ;; New modeline format does not fit on a smaller window
    (my-bound-cond
     (modeline-buffer-id
      (setq-default
       modeline-buffer-id
       (list (cons modeline-buffer-id-left-extent 'modeline-buffer-id-left)))))
    )))

;;}}}

;;{{{ Windowing System Customization

(when window-system (load "window-config"))

;; Do this *after* setting the modeline colours
(when (fboundp 'display-time)
  (display-time))

;;}}}


;;{{{ Keys

(unless (fboundp 'find-tag-at-point)
  (eval-when-compile (require 'etags))

  ;; Mimics version from XEmacs 21.2
  (defun find-tag-at-point ()
    "*Find tag whose name contains TAGNAME.
Identical to `find-tag' but does not prompt for tag when called interactively;
instead, uses tag around or before point."
    (interactive)
    (find-tag (if current-prefix-arg
		  (find-tag-tag "Find tag: "))
	      (find-tag (find-tag-default)))))

(defun my-buffer-file-name ()
  (interactive)
  (message "%s:%s" host-name
	   (if buffer-file-name buffer-file-name (buffer-name))))

(my-feature-cond
  (xemacs
   ;; This should always do the right thing
   (global-set-key [(return)] 'newline-and-indent)
   (global-set-key [(linefeed)] 'newline))
  (t
   ;; For Emacs the above breaks the minibuffer.
   ;; Note: c-mode does java too.
   (add-hook 'c-initialization-hook
	     (lambda  () (define-key c-mode-base-map [(return)] 'newline-and-indent)))
   (add-hook 'emacs-lisp-mode-hook
	     (lambda () (define-key emacs-lisp-mode-map [(return)] 'newline-and-indent)))
   (add-hook 'sh-mode-hook
	     (lambda () (define-key sh-mode-map [(return)] 'newline-and-indent)))
   ))

(defun my-toggle-case-search ()
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (when (interactive-p)
    (message "Case sensitive search %s." (if case-fold-search "off" "on"))))

;;;; Function keys. Only f1 is bound in XEmacs. We move to shift-f1.
(global-set-key [(shift f1)]    (global-key-binding [f1]))
(global-set-key [XF86_Switch_VT_1] (global-key-binding [f1]))
(global-set-key [f1]            'find-file)
(global-set-key [f2]		'undo)
(global-set-key [(shift f2)]	'redo)
(global-set-key [XF86_Switch_VT_2] 'redo)
(global-set-key [f3]		'isearch-repeat-forward)
(global-set-key [(shift f3)]    'isearch-repeat-backward)
(global-set-key [XF86_Switch_VT_3] 'isearch-repeat-backward)
(global-set-key [f4]		'next-error)
(global-set-key [f5]		'query-replace)
(global-set-key [(shift f5)]    'query-replace-regexp)
(global-set-key [XF86_Switch_VT_5] 'query-replace-regexp)
(global-set-key [f6]		'ff-find-other-file)
(global-set-key [(shift f6)]    'my-buffer-file-name)
(global-set-key [XF86_Switch_VT_6] 'my-buffer-file-name)
(global-set-key [f7]		'compile)
(global-set-key [(shift f7)]    'make-clean)
(global-set-key [XF86_Switch_VT_7] 'make-clean)
(global-set-key [(control f7)]	'my-set-compile)
(global-set-key [f8]		'grep)
;; shift f8 taken
(global-set-key [(control f8)]	'my-checkpatch)
(global-set-key [f9]		'my-isearch-word-forward)
(global-set-key [(shift f9)]    'my-toggle-case-search)
(global-set-key [XF86_Switch_VT_9] 'my-toggle-case-search)
(global-set-key [f10]		'find-tag-at-point)
(global-set-key [(shift f10)]   'pop-tag-mark)
(global-set-key [XF86_Switch_VT_10] 'pop-tag-mark)
(global-set-key [(control f10)]	'lxr-at-point)
;; I keep f11 free for temporary bindings
(global-set-key [f12]		'revert-buffer)
(global-set-key [(shift f12)]	'lxr-next-defined)
(global-set-key [XF86_Switch_VT_12] 'lxr-next-defined)
(global-set-key [(control f12)] 'lxr-defined-at-point)

(when (string= (getenv "HOSTNAME") "lappy.seanm.ca")
  ;; SAM HACK - period key broken on lappy
  (global-set-key [f6] ".")
  (global-set-key [(shift f6)] ">"))

;; Tilt wheel on Logitech M500 + others
;;(global-set-key [button6] ')
;;(global-set-key [button7] ')

;; Side buttons on Logitech M500 + others
;; NOTE: Needs patch
(global-set-key [button8] 'yank)
(global-set-key [button9] 'kill-region)

;; C-h =
(when (featurep 'xemacs)
  (define-key help-map ?= #'introspect-cursor-position))

(global-set-key "\C-ck" 'browse-kill-ring)


(would-like 'lxr)

;; Cut and paste
(setq interprogram-cut-function nil)
(setq interprogram-paste-function nil)

;; iswitchb
(my-feature-cond
 (xemacs (iswitchb-default-keybindings))
 (t (iswitchb-mode 1)))
(global-set-key "\C-x\C-b"	'iswitchb-buffer)
(global-set-key "\C-x\C-l"	'list-buffers)

(global-set-key "\C-x\C-k"	'kill-buffer)

(would-like 'intellimouse)
;(when (fboundp 'mwheel-install)
;    (mwheel-install)
;    (setq mwheel-follow-mouse t))

;; -------------------------------------------------------
;; The standard blows away emacs just a little to easily
(defun my-save-buffers-kill-emacs ()
  (interactive)
  (when (or (not window-system) (y-or-n-p "Do you have to go? "))
    (save-buffers-kill-emacs)))

(global-set-key "\C-x\C-c"	'my-save-buffers-kill-emacs)
(global-set-key "\C-xw"	'what-line)

;;(global-set-key "\C-cd"		'dup-line)
(global-set-key "\M-#"		'my-calc)

(global-set-key [(iso-left-tab)] 'tab-to-tab-stop)

;;}}}

;;{{{ Programming Packages

;;; -------------------------------------------------------------------------
;; FONT LOCK
;; See font-lock.el for a description of why it is called font lock.

;; Load it now so we can modify the fonts
(require 'font-lock)

;; Maximum colour but minimum chatter
(setq-default font-lock-maximum-decoration t
	      font-lock-verbose nil
	      font-lock-maximum-size nil)

(when (featurep 'xemacs)
  ;; Of all the modes, font-lock *least* needs a modeline
  ;; indicator. If the buffer is colourful, font-lock is on.
  ;; The only thing you lose is the ability to toggle it.
  (let ((el (assq 'font-lock-mode minor-mode-alist)))
    (if el (setcdr el '("")))))

;; Change a couple of faces
(make-face-bold 'font-lock-function-name-face)
(set-face-foreground 'font-lock-function-name-face "blue")
;; Um, this is the default
;(set-face-foreground 'font-lock-string-face "green4")
(if window-system
    (set-face-foreground 'font-lock-comment-face "FireBrick")
  ;; Consoles have less colors to play with
  (set-face-foreground 'font-lock-comment-face "red")
  (set-face-foreground 'font-lock-string-face "green")
  (set-face-foreground 'font-lock-keyword-face "blue")
  (set-face-foreground 'font-lock-variable-name-face "purple")
  )

(defun my-set-face (face fg bg &optional prop)
  (set-face-foreground face fg)
  (set-face-background face bg)
  (when prop (set-face-property face prop t)))

;; -------------------------------------------------------------------------
;; font-lock-comment-warn
;; I want all comments with my initials (SAM) at the start to be very bold
(defface font-lock-comment-warn-face
  '((((class color))  (:foreground "red" :bold t :italic t))
    (((class grayscale) (background light))
     (:foreground "DimGray" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :bold t :italic t))
    (t (:bold t)))
  "Font Lock mode face used to highlight warning comments."
  :group 'font-lock-faces)

(my-feature-cond
 (xemacs
  (defun setup-font-lock-keywords ()
    (let ((c-regexp "\\(/\\*\\|//\\) ?\\(\\<SAM\\>\\)"))
      (setq c-font-lock-keywords-1
	    (append c-font-lock-keywords-1
		    (list (list c-regexp 2 'font-lock-comment-warn-face t))))
      (setq c-font-lock-keywords-2
	    (append c-font-lock-keywords-2
		    (list (list c-regexp 2 'font-lock-comment-warn-face t))))
      (setq c-font-lock-keywords-3
	    (append c-font-lock-keywords-3
		    (list (list c-regexp 2 'font-lock-comment-warn-face t))))

      (setq c++-font-lock-keywords-1
	    (append c++-font-lock-keywords-1
		    (list (list c-regexp 2 'font-lock-comment-warn-face t))))
      (setq c++-font-lock-keywords-2
	    (append c++-font-lock-keywords-2
		    (list (list c-regexp 2 'font-lock-comment-warn-face t))))
      (setq c++-font-lock-keywords-3
	    (append c++-font-lock-keywords-3
		    (list (list c-regexp 2 'font-lock-comment-warn-face t))))
      (when nil ;; SAM NOT YET
      (setq go-mode-font-lock-keywords
	    (append go-mode-font-lock-keywords
		    (list (list c-regexp 2 'font-lock-comment-warn-face t))))
      ) ;; SAM
      ))

  (let ((lisp-regexp "; ?\\(\\<SAM\\>\\)"))
    (setq lisp-font-lock-keywords-1
	  (append lisp-font-lock-keywords-1
		  (list (list lisp-regexp 1 'font-lock-comment-warn-face t))))
    (setq lisp-font-lock-keywords-2
	  (append lisp-font-lock-keywords-2
		  (list (list lisp-regexp 1 'font-lock-comment-warn-face t))))
    )
  ) ;; xemacs

(t ;; GNU emacs
 ;; SAM This *should* work for XEmacs too since XEmacs supports
 ;; font-lock-add-keywords but even the example doesn't work.
 (defun setup-font-lock-keywords ()
   (font-lock-add-keywords
    'c-mode
    '(("\\(/\\*\\|//\\) ?\\(\\<SAM\\>\\)" 2 'font-lock-comment-warn-face t)))
   (font-lock-add-keywords
    'c++-mode
    '(("\\(/\\*\\|//\\) ?\\(\\<SAM\\>\\)" 2 'font-lock-comment-warn-face t))))

 (font-lock-add-keywords
  'emacs-lisp-mode
  '(("; ?\\(\\<SAM\\>\\)" 1 'font-lock-comment-warn-face t)))
 (font-lock-add-keywords
  'sh-mode
  '(("# ?\\(\\<SAM\\>\\)" 1 'font-lock-comment-warn-face t)))
 (font-lock-add-keywords
  'makefile-mode
  '(("# ?\\(\\<SAM\\>\\)" 1 'font-lock-comment-warn-face t)))
 ))

;; -------------------------------------------------------------------------
;; KSH MODE

;; sh-mode doesn't work too well in XEmacs. It doesn't handle case
;; labels properly. GNU Emacs handles case labels, but doesn't indent
;; comments properly. ksh-mode seems to handle case labels and
;; comments, so let's switch to that if it is available.
;;
;; ksh-mode not avaliable in Emacs, and turning it on loses font-lock
;; and bracket matching... so enable it only for xemacs for now

(my-feature-cond
 (xemacs
  (defun sh-to-ksh (entry)
    (when (eq (cdr entry) 'sh-mode)
      (setcdr entry 'ksh-mode))
    entry)

  (when (would-like 'ksh-mode)
    ;; Convert sh-mode to ksh-mode
    (mapc 'sh-to-ksh auto-mode-alist)
    (mapc 'sh-to-ksh interpreter-mode-alist)

    (setq ksh-indent 4)

    (nconc
     ksh-font-lock-keywords
     (list (list "# ?\\(\\<SAM\\>\\)" 1 'font-lock-comment-warn-face t)))
    )))

;; -------------------------------------------------------------------------
;; LISP MODE

(defun my-byte-compile-buffer ()
  "Byte compile and load the current buffer.
If `compilation-ask-about-save' is nil, saves the file without asking."
  (interactive)
  (save-some-buffers (not compilation-ask-about-save))
  (emacs-lisp-byte-compile-and-load))

(defun my-emacs-lisp-mode-hook ()
  ;; greedy-delete
  (when (would-like 'greedy-delete)
    (setq gd-indicator-string nil)
    (gd-add-to-mode))
  ;; Redefine compile
  (define-key emacs-lisp-mode-map [f7] 'my-byte-compile-buffer)
  )
(add-hook 'emacs-lisp-mode-hook 'my-emacs-lisp-mode-hook)

;;; -------------------------------------------------------------------------
;; compile variables
(setq compilation-read-command   nil
      compilation-ask-about-save nil
      compilation-window-height  12
      compilation-error-regexp-systems-list '(gnu)
      compile-command "make ")

(defun my-do-compile (cmd)
  (save-some-buffers (not compilation-ask-about-save) nil)
  (my-feature-cond
   (xemacs (compile-internal cmd "No more errors"))
   (emacs  (compilation-start cmd))))

;; This gives the compilation buffer its own frame
;;(push "*compilation*" special-display-buffer-names)

(defun my-set-compile ()
  (interactive)
  (require 'compile)
  (let ((cmd (read-string "Compile Command: " compile-command)))
    (make-local-variable 'compile-command)
    (setq compile-command cmd)
    (my-do-compile compile-command)))

;;----------------------------------------------------------------
(defvar make-clean-command "make clean all"
  "*Command used by the `make-clean' function.")

(defun make-clean (&optional arg)
  "Run a make clean."
  (interactive "P")
  (require 'compile) ;; needed for compile-internal
  (if arg
      (setq make-clean-command (read-string "Command: " make-clean-command)))
  (my-do-compile make-clean-command))

;;; -------------------------------------------------------------------------
;; hide-copyleft
;; If you're sure you're not gonna get sued, you can do something like this
;; in your .emacs file:
(when (would-like 'hide-copyleft)
  (add-to-list
   'copylefts-to-hide
   ;; Apache
   '(" \\* The Apache Software License, Version 1\\.1" . " \\*/")
   )
  (add-hook 'emacs-lisp-mode-hook 'hide-copyleft-region)
  (add-hook 'c-mode-common-hook 'hide-copyleft-region))

;;; -------------------------------------------------------------------------
;; tags
(setq tags-build-completion-table nil ;; too slow
      tags-auto-read-changed-tag-files t)

;;; -------------------------------------------------------------------------
;; GNU global - gtags
(let ((gtag-dir "/usr/share/gtags")) ;; default install location
  (when (file-directory-p gtag-dir)
    (add-to-list 'load-path gtag-dir)
    (autoload 'gtags-mode "gtags" "" t)))

;;; -------------------------------------------------------------------------
;; c macro expansion

(defun c-macro-expand-at-point (subst)
  (interactive "P")
  (let (start end)
    (if (region-exists-p)
	(setq start (region-beginning)
	      end (region-end))
      ;; symbol-near-point from 21.2-b45
      (save-excursion
	(if (or (bobp) (not (memq (char-syntax (char-before)) '(?w ?_))))
	    (while (not (looking-at "\\sw\\|\\s_\\|\\'"))
	      (forward-char 1)))
	(while (looking-at "\\sw\\|\\s_")
	  (forward-char 1))
	(when (re-search-backward "\\sw\\|\\s_" nil t)
	  (forward-char 1)
	  (setq end (point))
	  (forward-sexp -1)
	  (while (looking-at "\\s'")
	    (forward-char 1))
	  (setq start (point)))))
    ;; end of symbol-near-point
    (c-macro-expand start end subst)))

;;; -------------------------------------------------------------------------
(unless running-windoze (would-like 'svn))

(defvar signed-off-by-sig nil
  "* Signature used by `signed-off-by' function.
If nil, defaults to \"`user-full-name' <`user-mail-address'>\".")

(defun signed-off-by ()
  (interactive)
  (let ((signed-by (if signed-off-by-sig
		       signed-off-by-sig
		     (concat user-full-name " <" user-mail-address ">"))))
    (save-excursion
      (beginning-of-line)
      (insert (concat "Signed-off-by: " signed-by "\n---\n")))))

;;}}}

;;{{{ Handy Dandy(tm) Functions

(defun unixtime (seconds)
  (interactive "sTime: ")
  ;; Force it to a float for 32-bit systems.
  (let ((time (seconds-to-time (string-to-number (concat seconds ".0")))))
    (message "%s" (format-time-string "%a %b %d %T %Z %Y" time))))

(defun append-to-list (list-var element)
  "Append to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'."
  (or (member element (symbol-value list-var))
      (set list-var (nconc (symbol-value list-var) (list element)))))

(defun my-x-colour (number)
  (interactive "sColour: ")
  (cond
   ;; Convert 'd d d' to `#xxxxxx'
   ((string-match "^\\([0-9]+\\)\\([ \t]+[0-9]+\\)\\([ \t]+[0-9]+\\)$" number)
    (message "#%02x%02x%02x"
	     (string-to-number (match-string 1 number))
	     (string-to-number (match-string 2 number))
	     (string-to-number (match-string 3 number))))
   ;; Convert `#xxxxxx' to `d d d'
   ((string-match (concat "^#"
			  "\\([0-9a-fA-F][0-9a-fA-F]\\)"
			  "\\([0-9a-fA-F][0-9a-fA-F]\\)"
			  "\\([0-9a-fA-F][0-9a-fA-F]\\)$") number)
    (message "%d %d %d"
	     (string-to-number (match-string 1 number) 16)
	     (string-to-number (match-string 2 number) 16)
	     (string-to-number (match-string 3 number) 16)))
   (t (error "Invalid"))))

;;; -------------------------------------------------------------------------
;;  isearch "stuff"

;;  I don't like the way isearch-yank-word defines word, so I rolled my own
(defun my-isearch-yank-word ()
  "Pull current word from buffer into search string.
Use region if it exists. My replacement for isearch-yank-word."
  (interactive)
  (let ((word (if (region-exists-p)
		  (buffer-substring (region-beginning) (region-end))
		(current-word))))
    (forward-char 1) ;; make sure we are not on first char of word
    (my-feature-cond
     (xemacs (isearch-yank word))
     (t (isearch-yank-string word)))))

;; Warning: If you change this binding, change `my-isearch-word-forward'
(define-key isearch-mode-map "\C-w"		'my-isearch-yank-word)

(define-key isearch-mode-map [f3]		'isearch-repeat-forward)
(define-key isearch-mode-map [(shift f3)]	'isearch-repeat-backward)
(define-key isearch-mode-map "\C-t"		'isearch-toggle-case-fold)

(defun my-isearch-word-forward (&optional regexp-p)
  "Search for current word. Region is used if set."
  (interactive "P")
  ;; Push the C-w and call 'isearch-forward'
  (setq unread-command-events
	(my-feature-cond
	 (xemacs
	  (list (make-event 'key-press '(key ?w modifiers (control)))))
	 (t (listify-key-sequence "\C-w"))))
  (isearch-mode t (not (null regexp-p)) nil (not (interactive-p))))

;;; -------------------------------------------------------------------------
(when (packagep 'igrep running-windoze)
  ;;(igrep-insinuate)
  (global-set-key [f8] 'igrep)
  (global-set-key [(shift f8)] 'igrep-find)
  (setq igrep-verbose-prompts nil)
  (put 'igrep-files-default 'c-mode (lambda () "*.[ch]"))
  (put 'igrep-files-default 'emacs-lisp-mode (lambda () "*.el"))

  (defadvice igrep (before windowize activate)
    "This removes a final false match from `igrep' on the finished
line with `next-error'."
    (setq compilation-finish-function
	  '(lambda (buf status)
	     (save-excursion
	       (set-buffer buf)
	       (save-excursion
		 (goto-char (point-min))
		 ;; Emacs has a "started" line and will have text
		 ;; after "finished" and before "at".
		 (while (re-search-forward "^Igrep \\(started\\|finished\\) .*$" nil t)
		   (replace-match ""))))
	     (setq compilation-finish-function nil))))
)

;; For ispell
(setq ispell-silently-savep t
      ispell-extra-args '("-W" "3"))

;; For aspell
(when (exec-installed-p "aspell")
  (setq-default ispell-program-name "aspell"))

;; For flyspell
(when (would-like 'flyspell)
  (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
  (add-hook 'lisp-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)
  )

(defvar commit-names '("COMMIT_EDITMSG" "svn-commit.tmp")
  "* List of commit buffer names.")

(defun check-for-commit ()
  "If this is a commit buffer, set to text mode."
  (when (eq major-mode 'fundamental-mode)
    (let ((buff (buffer-name)))
      (dolist (name commit-names)
	(when (string= buff name)
	  (text-mode))))))
(add-hook 'find-file-hooks 'check-for-commit t)

;;; -------------------------------------------------------------------------
;; For when you need a good excuse...

(defvar excuse-phrase-file
  (concat dot-dir "site-packages/etc/excuses.lines")
  "*File containing excuses")

(defun excuse (&optional insert)
  "Return or display a random excuse.  With prefix arg, insert it."
  (interactive "P")
  (let ((case-fold-search nil)
	(excuse (concat (cookie excuse-phrase-file "I didn't" "do it"))))
    (if (string-match "^[^A-Z]" excuse)
	(setq excuse (concat "The problem is " excuse)))
    (if (string-match "[^.!?]$" excuse)
	(setq excuse (concat excuse ".")))
    (if insert
	(insert excuse)
      (message excuse))))

;;; -------------------------------------------------------------------------
(defun dup-line (&optional arg)
  "Duplicate the current line.
A negative arg comments out the `new' line[s]."
  (interactive "*p")
  (let ((line (buffer-substring
	       (progn (end-of-line) (point))
	       (progn (beginning-of-line) (point)))))
    ;; The above leaves the point at the start of the current line
    (dotimes (iter arg) (insert line) (newline))))

;;}}}

;;{{{ Packages

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; tramp needs this
;; (subtract-time '(13818 19266) '(13818 19145))
;; => (0 121)
(unless (fboundp 'subtract-time)
  ;; This is just time-sub from comics.el
  (defun subtract-time (t1 t2)
    "Subtact two lisp time values.
The time values are stored as a list of three 16-bit numbers.
We ignore the 3rd number."
  (let ((t1-lo (cadr t1))
	(t1-hi (car t1))
	(t2-lo (cadr t2))
	(t2-hi (car t2)))
    (when (> t2-lo t1-lo)
	(setq t1-hi (- t1-hi 1))
	(setq t1-lo (+ t1-lo 65536)))
    (list (- t1-hi t2-hi) (- t1-lo t2-lo)))))

;; for pui-list-packages
;;(if t
;;    ;; remote
;;    (setq package-get-remote '(("ftp.xemacs.org" "/pub/xemacs/packages")))
;;  ;; local
;;  (setq package-get-remote '((nil "~/.xemacs"))))
;;(setq package-get-remote '(("ftp.xemacs.org" "/pub/xemacs/beta/experimental")))
(setq package-get-require-signed-base-updates nil)
;; Turn off the automatic ftp of missing packages
;;(fset 'package-get-package-provider 'ignore)

;;; -------------------------------------------------------------------------
;; The auto-save.el and backup.el packages collect files in one place
;; I added the following to my crontab:
;; # Cleanup the autosave and backup directories (24 * 7 = 168)
;; 0 4 * * * /usr/sbin/tmpwatch 168 $HOME/.autosave $HOME/.backup

(my-feature-cond
 (xemacs
  (when (would-like 'auto-save)
    (setq auto-save-directory "~/.autosave/")
    ;; Now that we have auto-save-timeout, let's crank this up
    ;; for better interactive response.
    (setq auto-save-interval 2000))
  (would-like 'backup))
 (t
  (setq backup-directory-alist '(("." . "~/.backup")))))

;;; -------------------------------------------------------------------------
;;; Filladapt is a syntax-highlighting package.  When it is enabled it
;;; makes filling (e.g. using M-q) much much smarter about paragraphs
;;; that are indented and/or are set off with semicolons, dashes, etc.

(when (would-like 'filladapt)
  (add-hook 'text-mode-hook 'turn-on-filladapt-mode)
  (add-hook 'mail-mode-hook 'turn-on-filladapt-mode))

;;; ------------------------------------------------------------
;; Start the server program
(unless (or running-windoze running-as-root)
  (my-feature-cond
   (xemacs (gnuserv-start)
	   (setq gnuserv-frame (selected-frame)))
   (t (server-start))))

;;; ----------------------------------------------
;; Whitespace mode handy for tabs - ignore spaces
;; From text-modes
(setq whitespace-chars 'tabs)
(setq whitespace-install-submenu t)

;;; ----------------------------------------------
;; These come from the site-lisp directory

;; ws-trim-mode
(when (would-like 'ws-trim)
  (global-ws-trim-mode t)
  (setq ws-trim-mode-line-string nil)
  (set-default 'ws-trim-level 1))

(when (would-like 'whitespace)
  (my-feature-cond (xemacs (whitespace-global-mode))))

;;}}}

;;{{{ Mail

;; Authorization in .authrc

(when (would-like 'sendmail)
  (setq mail-user-agent 'sendmail-user-agent)
  (setq user-mail-address (concat (user-login-name) "@" domain-name))
  ;;(setq smtpmail-debug-info t)
  )

;; Domain specific mail
(let ((domain-specific-init (concat dot-dir "mail-" domain-name)))
  (when (file-exists-p domain-specific-init)
    (load domain-specific-init)))

;;}}}

;; If we don't have el-rcfiles then just load all the files now
(unless (boundp 'rcfiles-version)
  (dolist (rcfile (directory-files rcfiles-directory nil ".*-rc.el"))
    (when (string-match "\\(.*\\\)-rc.el" rcfile)
      (load (match-string 1 rcfile)))
    (load rcfile)))

(unless noninteractive
  (when running-windoze
    (load (concat dot-dir "windoze") t))

  (load (concat dot-dir "work") t)

  ;; I use a common init.el across many machines. The `user-init' file
  ;; allows for user/machine specific initialization.
  (unless running-as-root
    (load (concat dot-dir "user-init") t)))

(setq initial-scratch-message
      ";; This buffer is for goofing around in.
;; All data will be destroyed on exit.

")

;;{{{ Final results

(setq debug-on-error nil)

;; Every time you turn around Emacs is displaying yet another
;; stupid^h^h^h^h^h useful message that overwrites my nice friendly
;; one. So use a timer to get past them.
(unless noninteractive
  (start-itimer "delayed-msg"
		(lambda ()
		  (if would-have-liked-list
		      ;; Warn that some features not found
		      (progn (ding)
			     (message "Features not found: %S" would-have-liked-list))
		    ;; Else display a friendly message
		    (let ((hour (nth 2 (decode-time))))
		      (message "Good %s %s"
			       (cond ((< hour 12) "morning")
				     ((< hour 18) "afternoon")
				     (t           "evening"))
			       (user-full-name))))
		  (delete-itimer "delayed-msg"))
		1))

;;}}}

;; end of .emacs "May the `(' be with `)'"
