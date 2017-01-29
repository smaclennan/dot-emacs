;; S?X?Emacs setup -*- Mode:emacs-lisp -*-
;; This file should work with XEmacs 2x.x, Emacs >= 21.x, or SXEmacs

;; Aug 9, 2013: Can run with no packages.

;;{{{ Configuration variables / functions

; This is the one key binding I must have... switch ASAP
(global-set-key "\C-x\C-b" 'switch-to-buffer)

(defvar running-windoze (eq system-type 'windows-nt)
  "Non-nil if running Windows.")

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

;; We need to setup for GNU Emacs
(if (not (featurep 'xemacs)) (load (concat dot-dir "esp/esp")))

(when (featurep 'sxemacs)
  (setq load-path (append load-path
			  (list (concat dot-dir "/site-packages/lisp/sam"))
			  (list (concat dot-dir "/site-packages/lisp/misc"))))
  )

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
  (if (boundp 'packages-package-list)
      (if (assq package packages-package-list)
	  t
	(unless no-list (add-to-list 'would-have-liked-list package))
	nil)
    (would-like package no-list)))

;; Split the system-name up into host and domain name.
;; We need this up front for sendmail-rc.
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

;; For rcfiles to be able to match loaded lisp such as lisp-mode we
;; need to turn the file names into simple load names.
(setq load-history
      (mapcar (lambda (elt)
		(let ((a (car elt)))
		  (if (eq 0 (string-match "/.*/\\([^/]+\\)\.elc" a))
		      (list (match-string 1 a))
		    (list a))))
	      load-history))

;; The standard doesn't support sxemacs
(setq rcfiles-directory (concat dot-dir "rc"))
(if (would-like 'rcfiles)
    (rcfiles-register-rc-files)
  (load (concat dot-dir "esp/rcfiles")))

;;}}}

;;{{{ Basic Customization

;; Default the package location
(when running-xemacs
  (setq package-get-remote
;;	'("ftp.ca.xemacs.org" "/pub/Mirror/xemacs/beta/experimental/packages")))
	'("ftp.xemacs.org" "/pub/xemacs/xemacs-21.5/experimental/packages")))

(setq track-eol t
      kill-whole-line t
      next-line-add-newlines nil
      delete-key-deletes-forward t
      find-file-compare-truenames t
      signal-error-on-buffer-boundary nil
      inhibit-default-init t
      inhibit-startup-message t
      visible-bell t
      initial-scratch-message ";; This buffer is for goofing around in.\n\n")

(put 'narrow-to-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

;; Let's try making _ part of a "word". C & C++ done in cc-mode-rc.el
(modify-syntax-entry ?_ "w" (standard-syntax-table))

;; (would-like 'uncompress) ;; os-utils
(and (not noninteractive)
     (would-like 'jka-compr)
     (auto-compression-mode 1))

;; Always turn this mode off
(fset 'xrdb-mode 'ignore)

;; This is defined in apel - here is a simple version
(unless (fboundp 'exec-installed-p)
  (defun exec-installed-p (file)
    "Return absolute-path of FILE if FILE is executable.
Local version."
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

;;}}}

;;{{{ XEmacs 21.5 stuff

(and running-xemacs (emacs-version>= 21 5)
     ;; For some reason the file coding was gutted - put it back
     (setq buffer-file-coding-system-for-read 'undecided
	   default-buffer-file-coding-system  'raw-text))

;;}}}

;;{{{ Windowing System Customization

(if window-system
    (load (concat rcfiles-directory "/" "window-config"))
  ;; Yes, emacs has a menu bar in console mode
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode -1)))

;;}}}


;;{{{ Keys

(if running-xemacs
    (progn
      ;; This should always do the right thing
      (global-set-key [(return)] 'newline-and-indent)
      (global-set-key [(linefeed)] 'newline))
  ;; For Emacs the above breaks the minibuffer.
  ;; Note: c-mode does java too.
  (add-hook 'c-initialization-hook
	    (lambda  () (define-key c-mode-base-map [(return)] 'newline-and-indent)))
  (add-hook 'emacs-lisp-mode-hook
	    (lambda () (define-key emacs-lisp-mode-map [(return)] 'newline-and-indent)))
  (add-hook 'sh-mode-hook
	    (lambda () (define-key sh-mode-map [(return)] 'newline-and-indent)))
  )

;;;; Function keys. Only f1 is bound in XEmacs. We move it to shift-f1.
(global-set-key [(shift f1)]    (global-key-binding [f1]))
(global-set-key [XF86_Switch_VT_1] (global-key-binding [f1]))
(global-set-key [f1]            'find-file)
(global-set-key [f2]		'undo)
(global-set-key [(shift f2)]	'redo)
(global-set-key [XF86_Switch_VT_2] 'redo)
; f3 is isearch
(global-set-key [f4]		'next-error)
(global-set-key [f5]		'query-replace)
(global-set-key [(shift f5)]    'query-replace-regexp)
(global-set-key [XF86_Switch_VT_5] 'query-replace-regexp)
(global-set-key [f6]		'git-grep-at-point)
(global-set-key [(shift f6)]	'git-grep)
(global-set-key [XF86_Switch_VT_6] 'git-grep)
(global-set-key [f7]		'compile)
(global-set-key [(shift f7)]    'make-clean)
(global-set-key [XF86_Switch_VT_7] 'make-clean)
(global-set-key [(control f7)]	'my-set-compile)
(global-set-key [f8] 'igrep)
(global-set-key [(shift f8)] 'igrep-find)
(global-set-key [(control f8)]	'my-checkpatch)
; f9 is isearch
(global-set-key [f10]		'find-tag-at-point)
(global-set-key [(shift f10)]   'pop-tag-mark)
(global-set-key [XF86_Switch_VT_10] 'pop-tag-mark)
(global-set-key [f20] 'pop-tag-mark)
(global-set-key [(control f10)]	'lxr-at-point)
;; I keep f11 free for temporary bindings
(global-set-key [f12]		'revert-buffer)
(global-set-key [(shift f12)]	'lxr-next-defined)
(global-set-key [XF86_Switch_VT_12] 'lxr-next-defined)
(global-set-key [(control f12)] 'lxr-defined-at-point)

(global-set-key [f3]		'isearch-repeat-forward)
(global-set-key [(shift f3)]    'isearch-repeat-backward)
(global-set-key [XF86_Switch_VT_3] 'isearch-repeat-backward)
(global-set-key [f9]		'my-isearch-word-forward)
(global-set-key [(shift f9)]    'my-toggle-case-search)
(global-set-key [XF86_Switch_VT_9] 'my-toggle-case-search)

(global-set-key "\C-ce" 'errno-string)
(global-set-key "\C-cg" 'git-diff)
(global-set-key "\C-ck" 'browse-kill-ring)


;;  I don't like the way isearch-yank-word defines word, so I rolled my own
(defun my-isearch-yank-word ()
  "Pull current word from buffer into search string.
Use region if it exists. My replacement for isearch-yank-word."
  (interactive)
  (let ((word (if (region-exists-p)
		  (buffer-substring (region-beginning) (region-end))
		(current-word))))
    (forward-char 1) ;; make sure we are not on first char of word
    (if (fboundp 'isearch-yank)
	(isearch-yank word)
      (isearch-yank-string word))))

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
	(if running-xemacs
	    (list (make-event 'key-press '(key ?w modifiers (control))))
	  (listify-key-sequence "\C-w")))
  (isearch-mode t (not (null regexp-p)) nil (not (interactive-p))))

(defun my-toggle-case-search ()
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (when (interactive-p)
    (message "Case sensitive search %s." (if case-fold-search "off" "on"))))

;; Tilt wheel on Logitech M500 + others
;;(global-set-key [button6] ')
;;(global-set-key [button7] ')

;; Side buttons on Logitech M500 + others
;; NOTE: Needs patch
(global-set-key [button8] 'yank)
(global-set-key [button9] 'kill-region)

;; C-h =
(when running-xemacs
  (define-key help-map ?= #'introspect-cursor-position))

(global-set-key "\C-x\C-l"	'list-buffers)

(global-set-key "\C-x\C-k"	'kill-buffer)

(would-like 'intellimouse)
;(when (fboundp 'mwheel-install)
;    (mwheel-install)
;    (setq mwheel-follow-mouse t))

;; Fixup GNU Emacs
(when (not running-xemacs)
  (global-set-key [M-right] 'forward-sexp)
  (global-set-key [M-left]  'backward-sexp)
  )

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

;; Always want font-lock
;; Use require. (turn-on-font-lock) caused no end of grief on my work computers.
(unless noninteractive (require 'font-lock))

;; -------------------------------------------------------------------------
;; KSH MODE

;; sh-mode doesn't work too well in XEmacs. It doesn't handle case
;; labels properly. GNU Emacs handles case labels, but doesn't indent
;; comments properly. ksh-mode seems to handle case labels and
;; comments, so let's switch to that if it is available.
;;
;; ksh-mode not avaliable in Emacs, and turning it on loses font-lock
;; and bracket matching... so enable it only for xemacs for now

(when running-xemacs (would-like 'ksh-mode))

;;; -------------------------------------------------------------------------
;; hide-copyleft
;; If you're sure you're not gonna get sued, you can do something like this
;; in your .emacs file:
(when running-xemacs
  (when (would-like 'hide-copyleft)
    (add-to-list
     'copylefts-to-hide
     ;; Apache
     '(" \\* The Apache Software License, Version 1\\.1" . " \\*/")
     )
    (add-hook 'emacs-lisp-mode-hook 'hide-copyleft-region)
    (add-hook 'c-mode-common-hook 'hide-copyleft-region)))

;;; -------------------------------------------------------------------------
;; GNU global - gtags
(let ((gtag-dir "/usr/share/gtags")) ;; default install location
  (when (file-directory-p gtag-dir)
    (add-to-list 'load-path gtag-dir)
    (autoload 'gtags-mode "gtags" "" t)))

;;; -------------------------------------------------------------------------
;; (unless running-windoze (would-like 'svn))

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

;;}}}

;;{{{ Handy Dandy(tm) Functions

(defun directory-files-recursive (dir &optional match)
  "Return a list of files in DIR recursively descending all
subdirectories that do not start in a dot (.). If MATCH is non-nil,
match all files against the regular expression."
  (let ((files (directory-files dir t match nil t)))
    (dolist (d (directory-files dir nil "^[^.].*" nil 'dirs))
      (setq files (append files (all-files (concat dir "/" d)))))
    files))

(defun unixtime (seconds)
  (interactive "sTime: ")
  ;; Force it to a float for 32-bit systems.
  (let ((time (string-to-number (concat seconds ".0"))))
    (message "%s"
	     (format-time-string
	      "%a %b %d %T %Z %Y"
	      ;; seconds-to-time from time-date.el in gnus
	      (list (floor time 65536)
		    (floor (mod time 65536))
		    (floor (* (- time (ffloor time)) 1000000)))
	      ))))

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

(defun load-path-roots ()
  "Show only the root dirs in the `load-path'."
  (interactive)
  (let (dirs)
    (dolist (dir load-path)
      (when (string-match "/lisp.*" dir)
	(setq dir (replace-match "" nil nil dir)))
      (add-to-list 'dirs dir))
    (if (interactive-p) (message "%S" dirs))
    dirs))

;;; -------------------------------------------------------------------------
;; For when you need a good excuse...

(defvar excuse-phrase-file (locate-data-file "excuses.lines")
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

;;; -------------------------------------------------------------------------
;;; Some edit-utils packages
(when (or (not running-xemacs) (packagep 'edit-utils))
  (if running-xemacs
      (progn
	(paren-set-mode 'paren t)
	(iswitchb-default-keybindings)
	(would-like 'redo))
    (show-paren-mode t)
    (if (fboundp 'ido-mode)
	(ido-mode 1)
      (require 'iswitchb)
      (iswitchb-mode 1)))

  (global-set-key "\C-x\C-b" (global-key-binding "\C-xb"))

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'post-forward)
  )

;;; -------------------------------------------------------------------------
;;; Some text-modes packages
(when (or (not running-xemacs) (packagep 'text-modes))
  (add-hook 'text-mode-hook 'turn-on-auto-fill)

  ;; Filladapt is a syntax-highlighting package.  When it is enabled it
  ;; makes filling (e.g. using M-q) much much smarter about paragraphs
  ;; that are indented and/or are set off with semicolons, dashes, etc.
  (defun add-filladapt()
	(require 'filladapt) ;; No autoloads
	(turn-on-filladapt-mode))
  (add-hook 'text-mode-hook 'add-filladapt)
  (add-hook 'mail-mode-hook 'add-filladapt)

  ;; Flyspell
  (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
  (add-hook 'lisp-mode-hook 'flyspell-prog-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)

  ;; (when (fboundp 'whitespace-global-mode) (whitespace-global-mode))
  )

;;; -------------------------------------------------------------------------
;; The auto-save.el and backup.el packages collect files in one place
;; I added the following to my crontab:
;; 13 5 * * * find $HOME/.backup -mtime +7 -delete

(if running-xemacs
    (progn
      (when (would-like 'auto-save)
	(setq auto-save-directory "~/.autosave/")
	;; Now that we have auto-save-timeout, let's crank this up
	;; for better interactive response.
	(setq auto-save-interval 2000))
      (would-like 'backup))
  (setq backup-directory-alist '(("." . "~/.backup"))))

;;; ----------------------------------------------
;; ws-trim-mode
(when (fboundp 'global-ws-trim-mode)
  (global-ws-trim-mode t)
  (setq ws-trim-mode-line-string nil)
  (set-default 'ws-trim-level 1))

;;}}}

;;; ------------------------------------------------------------
;; Start the server program
(unless (or noninteractive running-windoze (string= (user-login-name) "root"))
  (if running-xemacs
      (progn
	(gnuserv-start)
	(setq gnuserv-frame (selected-frame)))
      (server-start)))

;;; ------------------------------------------------------------
;; Some non-standard init files. Start them last so they can override defaults.
(when running-windoze (load "windoze"))

(load (concat dot-dir "work") t)

;; I use a common init.el across many machines. The `user-init' file
;; allows for user/machine specific initialization.
(load (concat dot-dir "user-init") t)

;;{{{ Final results

(defun friendly-message (&optional full)
  (interactive "P")
  (if (and full would-have-liked-list)
      ;; Warn that some features not found
      (progn (ding)
	     (message "Features not found: %S" would-have-liked-list))
    ;; Else display a friendly message
    (let ((hour (nth 2 (decode-time))))
      (message "Good %s %s"
	       (cond ((< hour 12) "morning")
		     ((< hour 18) "afternoon")
		     (t           "evening"))
	       (user-full-name)))))

;; Every time you turn around Emacs is displaying yet another
;; stupid^h^h^h^h^h useful message that overwrites my nice friendly
;; one. So use a timer to get past them.
(unless noninteractive
  (start-itimer "delayed-msg" 'friendly-message 1 nil nil t t))

;;}}}

;; end of .emacs "May the `(' be with `)'"
