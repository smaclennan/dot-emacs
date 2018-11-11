;; S?X?Emacs setup -*- Mode:emacs-lisp -*-
;; This file should work with XEmacs 2x.x, Emacs >= 22.1, or SXEmacs

;;{{{ Configuration variables / functions

; This is the one key binding I must have... switch ASAP
(global-set-key "\C-x\C-b" 'switch-to-buffer)

;;(setq debug-on-error t)

(defvar running-windoze (eq system-type 'windows-nt)
  "Non-nil if running Windows.")

(defvar dot-dir (expand-file-name
		 (if (featurep 'xemacs) "~/.xemacs/" "~/.emacs.d/"))
  "The init file directory.")

(load (concat dot-dir (if (featurep 'xemacs) "xemacs/helper" "emacs/esp")))

;; With the new package system, there is a greater chance a
;; package may be missing. Instead of an error, just add the
;; package to a list of missing packages and move on.
;; Note: returns non-nil if package exists.

(defvar would-have-liked-list nil
  "List of features that `would-like' could not find.")

(defun would-like (feature &optional no-list)
  "A less strident `require'."
  (condition-case nil
      (require feature)
    (error
     (unless no-list (add-to-list 'would-have-liked-list feature))
     nil)))

(require 'sam-common)

;; Split the system-name up into host and domain name.
;; We need this up front for sendmail-rc.
(defvar host-name nil)
(defvar domain-name nil)
(let ((my-system-name (system-name)))
  (if (string-match "^\\([^.]+\\)\\.\\(.*\\)" my-system-name)
      ;; fully qualified system-name
      (setq host-name (match-string 1 my-system-name)
	    domain-name (match-string 2 my-system-name))
  ;; system-name is host-name
  (setq host-name my-system-name
	domain-name (getenv "DOMAINNAME"))))

;; I use a common init.el across many machines. The `user-init' file
;; allows for user/machine specific initialization. It must be very
;; early for variables like laptop-mode to work. Use `after-init-hook'
;; if you need to clean something up at the end.
(load (concat dot-dir "user-init") t)

;; The standard doesn't support sxemacs
(setq rcfiles-directory (concat dot-dir "rc/"))

(if (would-like 'rcfiles)
    (rcfiles-register-rc-files)
  (load (concat dot-dir "esp/rcfiles")))

;;}}}

;;{{{ Basic Customization

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

;; This has got to be the hardest variable to set. It seems you *must*
;; hardcode the name and it can't be in a compiled file. Yes, the
;; docs say it can, but the eval form does not work.
(cond
 ((equal (user-login-name) "sam")
  (setq inhibit-startup-echo-area-message "sam"))
 ((equal (user-login-name) "smaclennan")
  (setq inhibit-startup-echo-area-message "smaclennan"))
 ((equal (user-login-name) "seanm")
  (setq inhibit-startup-echo-area-message "seanm"))
 )

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

(defun my-exec-installed-p (file)
  "Return absolute-path of FILE if FILE is executable.
Simple version."
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
	  (throw 'found path))))))

;; Windowing System Customization

(if window-system
    (load (concat rcfiles-directory "window-config"))
  ;; Yes, emacs has a menu bar in console mode
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode -1)))

(defun size-window (size)
  (let* ((edges (window-edges))
	 (cursize (- (nth 3 edges) (nth 1 edges) -1)))
    (unless (= size cursize)
      (enlarge-window (- size cursize)))))

;;}}}

;;{{{ Keys

;; For Emacs this breaks the minibuffer. Emacs dealt with in rc/ files.
(when running-xemacs
  ;; This should always do the right thing
  (global-set-key [(return)] 'newline-and-indent)
  (global-set-key [(linefeed)] 'newline))

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
(global-set-key [(control f6)]	'git-grep-toggle-top-of-tree)
(global-set-key [XF86_Switch_VT_6] 'git-grep)
(global-set-key [f7]		'compile)
(global-set-key [(shift f7)]    'make-clean)
(global-set-key [XF86_Switch_VT_7] 'make-clean)
(global-set-key [(control f7)]	'my-set-compile)
(global-set-key [f8] 'my-grep)
(global-set-key [(shift f8)] 'my-grep-find)
(global-set-key [(control f8)]	'my-checkpatch)
; f9 is isearch
(global-set-key [f10]		'find-tag-at-point)
(global-set-key [(shift f10)]   'pop-tag-mark)
(global-set-key [XF86_Switch_VT_10] 'pop-tag-mark)
(global-set-key [f20] 'pop-tag-mark)
(global-set-key [(control f10)]	'my-cscope)
;; I keep f11 free for temporary bindings
(global-set-key [(shift f11)] 'my-show-messages)
(global-set-key [XF86_Switch_VT_11] 'my-show-messages)
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

(global-set-key "\C-cd" 'dup-line)
(global-set-key "\C-ce" 'errno-string)
(global-set-key "\C-cg" 'git-diff)
(global-set-key "\C-ck" 'browse-kill-ring)
(global-set-key "\C-co" 'ogrok)

(global-set-key "\C-c8" '80-scan)
(global-set-key "\C-c9" '80-cleanup) ;; shift-8 and ctrl-8 did not work

(when (not running-xemacs)
  (defun xref-find-def ()
    "Call `xref-find-definition' but always prompt for identifier."
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively 'xref-find-definitions)))
  (global-set-key "\M-." 'xref-find-def))

(defun lxr-or-ogrok-at-point ()
  (interactive)
  (if (and ogrok-url ogrok-project)
      (ogrok-at-point)
    (lxr-at-point)))

(defun my-show-messages ()
  "Show messages in other window."
  (interactive)
  (my-feature-cond
    (xemacs (show-message-log))
    (t (switch-to-buffer-other-window "*Messages*"))
    ))

(defun grab-word ()
  "Grab the word on or after the point."
  (interactive)
  (let (end)
    (save-excursion
      (if (forward-word)
	  (progn ;; normal case
	    (setq end (point))
	    (backward-word))
	;; special case when point past last word in buffer
	(backward-word)
	(setq end (point))
	(forward-word))
      (copy-region-as-kill (point) end))))

;; emacs has problems with \C-,
(global-set-key [(control ?.)] 'grab-word)

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
      (isearch-yank (isearch-yank word))
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
  (my-feature-cond
    (xemacs
     (setq unread-command-events
	   (list (make-event 'key-press '(key ?w modifiers (control))))))
    (t
     (setq unread-command-events
	   (listify-key-sequence "\C-w"))))
  (isearch-mode t (not (null regexp-p)) nil (not (my-interactive-p))))

(defun my-toggle-case-search ()
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (when (my-interactive-p)
    (message "Case sensitive search %s." (if case-fold-search "off" "on"))))

;; Tilt wheel on Logitech M500 + others
;;(global-set-key [button6] ')
;;(global-set-key [button7] ')

;; Side buttons on Logitech M500 + others
;; NOTE: Needs patch
(global-set-key [button8] 'yank)
(global-set-key [button9] 'kill-region)

;; C-h =
(my-feature-cond
  (xemacs (define-key help-map ?= #'introspect-cursor-position)))

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

(global-set-key "\M-#"		'my-calc)

(global-set-key [(iso-left-tab)] 'tab-to-tab-stop)

;;}}}

;;{{{ Programming Packages

;; Always want font-lock
;; Use require. (turn-on-font-lock) caused no end of grief on my work computers.
(unless noninteractive (require 'font-lock))

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
;; GNU global - gtags
(let ((gtag-dir "/usr/share/gtags")) ;; default install location
  (when (file-directory-p gtag-dir)
    (add-to-list 'load-path gtag-dir)
    (autoload 'gtags-mode "gtags" "" t)))

;;; -------------------------------------------------------------------------
;; (unless running-windoze (would-like 'svn))

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
;; because this spans multiple packages... it makes sense to put it here

(defun my-tools-config (verbose)
  "Dump config information for the current buffer."
  (interactive "P")
  (require 'git-diff) ;; for git-dir
  (with-output-to-temp-buffer "*my-config*"
    ;; Common
    (princ (format "File name:    %S\n" (buffer-file-name)))
    (let ((git-dir (condition-case nil (git-dir) (error nil))))
      (princ (format "Git dir:      %S\n" git-dir)))
    (princ (format "Compile:      %S\n" compile-command))

    ;; C-ish files
    (when (and (boundp 'c-buffer-is-cc-mode) c-buffer-is-cc-mode)
      (princ (format "C Style:      %S" c-indentation-style))
      (princ (if indent-tabs-mode " tabs " " spaces "))
      (if (eq c-basic-offset tab-width) ;; 99.99% case
	  (princ (format "%d\n" c-basic-offset))
	(princ (format "%d/%d\n" c-basic-offset tab-width))))

    ;; optional files
    (if tags-file-name
	(princ (format "Tag file:     %S %s\n"
		       tags-file-name
		       (if (file-exists-p tags-file-name) "OK" "missing")))
      (if (file-exists-p "TAGS")
	  (princ (format "Tag file:     \"TAGS\"\n"))))
    (condition-case nil
	(when (or my-tags-dir my-tags-file)
	  (princ (format "My tag dir:   %S\n" my-tags-dir))
	  (princ (format "My tag file:  %S\n" my-tags-file)))
      (error nil))
    (condition-case nil
	(princ (format "Cscope dir:   %S\n" (mcs-dir)))
      (error nil))
    (when (local-variable-p 'kloc-dir)
      (princ (format "Kloc:         %S\n" kloc-dir)))

    ;; verbose and C
    (when (and verbose (boundp 'my-compile-dir-list))
      (require 'cl-extra)
      (princ "\nmy-compile-dir-list:")
      (princ (with-temp-buffer
	       (cl-prettyprint my-compile-dir-list)
	       (buffer-string))))
    ))

;;}}}

;;{{{ Packages

;;; -------------------------------------------------------------------------
;;; Some edit-utils packages
(my-feature-cond
  (xemacs
   (iswitchb-default-keybindings)
   (would-like 'redo))
;;;  (icomplete-mode
;;;   (icomplete-mode 1))
;;;  (ido-mode
;;;   (ido-mode 1))
  (t
   ;; We have a local copy of iswitchb to get around the deprecated message.
   (load (concat dot-dir "emacs/iswitchb"))
   (iswitchb-mode 1)))

(my-feature-cond
  (xemacs (paren-set-mode 'paren t))
  (t (show-paren-mode t)))

(global-set-key "\C-x\C-b" (global-key-binding "\C-xb"))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;;; -------------------------------------------------------------------------
;;; Some text-modes packages
(add-hook 'text-mode-hook 'turn-on-auto-fill)

(my-feature-cond
  (xemacs
   ;; Filladapt is a syntax-highlighting package.  When it is enabled it
   ;; makes filling (e.g. using M-q) much much smarter about paragraphs
   ;; that are indented and/or are set off with semicolons, dashes, etc.
   (defun add-filladapt()
     (require 'filladapt) ;; No autoloads
     (turn-on-filladapt-mode))
   (add-hook 'text-mode-hook 'add-filladapt)
   (add-hook 'mail-mode-hook 'add-filladapt)))

;; Flyspell
(if (or (my-exec-installed-p "hunspell") (my-exec-installed-p "aspell"))
    (progn
      (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
      (add-hook 'lisp-mode-hook 'flyspell-prog-mode)
      (add-hook 'text-mode-hook 'flyspell-mode))
  (add-to-list 'would-have-liked-list 'spell running-windoze))

;; (when (fboundp 'whitespace-global-mode) (whitespace-global-mode))

;;; -------------------------------------------------------------------------
;; The auto-save.el and backup.el packages collect files in one place
;; I added the following to my crontab:
;; 13 5 * * * find $HOME/.backup -mtime +7 -delete
;; 17 5 * * * find $HOME/.autosave -mtime +7 -delete

(my-feature-cond
  (xemacs (would-like 'backup))
  (t
   (setq auto-save-file-name-transforms `((".*" "~/.autosave/" t)))
   (setq backup-directory-alist '(("." . "~/.backup")))))

;;; ----------------------------------------------
;; ws-trim-mode
(when (fboundp 'global-ws-trim-mode)
  (global-ws-trim-mode t)
  (setq ws-trim-mode-line-string nil)
  (set-default 'ws-trim-level 1))

;;; ------------------------------------------------------------
;; Start the server program
(unless (or noninteractive running-windoze (string= (user-login-name) "root"))
  (my-feature-cond
    (xemacs
     (gnuserv-start)
     (setq gnuserv-frame (selected-frame)))
    (t (server-start))))

;;}}}

;;{{{ Optional Init files

;;; ------------------------------------------------------------
;; Some non-standard init files. Start them last so they can override defaults.
(when running-windoze (load "windoze"))

(load (concat dot-dir "work") t)

;;}}}

;;{{{ Final results

(defun friendly-message (&optional full)
  (interactive "P")
  (if (and full would-have-liked-list)
      ;; Warn that some features not found
      (message "Features not found: %S" would-have-liked-list)
    ;; Else display a friendly message
    (let ((hour (nth 2 (decode-time))))
      (message "Good %s %s"
	       (cond ((< hour 12) "morning")
		     ((< hour 18) "afternoon")
		     (t           "evening"))
	       (user-full-name)))))

;; I have been able to disable all the Emacs messages in non-console mode... so we
;; only need the timer for console modes.
(unless noninteractive
  (if (and (not running-xemacs) window-system)
      (friendly-message t)
    ;; Every time you turn around Emacs is displaying yet another
    ;; stupid^h^h^h^h^h useful message that overwrites my nice friendly
    ;; one. So use a timer to get past them.
    (run-at-time 1 nil 'friendly-message t)))

;;}}}

;; end of .emacs "May the `(' be with `)'"
