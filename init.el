;; S?X?Emacs setup -*- Mode:emacs-lisp -*-
;; This file should work with XEmacs 2x.x, Emacs >= 22.1, or SXEmacs

;;{{{ Configuration variables / functions

; This is the one key binding I must have... switch ASAP
(global-set-key "\C-x\C-b" 'switch-to-buffer)

(defvar dot-dir (expand-file-name "~/.emacs.d/")
  "The init file directory.")

(load (concat dot-dir "lisp/helper"))

;; With the new package system, there is a greater chance a
;; package may be missing. Instead of an error, just add the
;; package to a list of missing packages and move on.
;; Note: returns non-nil if package exists.

(defvar would-have-liked-list nil
  "List of features that `would-like' could not find.")

(defun would-like (feature)
  "A less strident `require'."
  (condition-case nil (require feature)
    (error
     (add-to-list 'would-have-liked-list feature) nil)))

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

(setq rcfiles-directory (concat dot-dir "rc/"))
(rcfiles-register-rc-files)

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

;; Windowing System Customization

(if window-system
    (load (concat rcfiles-directory "window-config"))
  ;; Yes, emacs has a menu bar in console mode
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode -1)))

;;}}}

;;{{{ Keys

;; For Emacs this breaks the minibuffer. Emacs dealt with in rc/ files.
(when running-xemacs
  ;; This should always do the right thing
  (global-set-key [(return)] 'newline-and-indent)
  (global-set-key [(linefeed)] 'newline))

;;;; Function keys.
(global-set-key [XF86_Switch_VT_1] (global-key-binding [f1]))
(global-set-key [f1]            'find-file)
(global-set-key [f2]		'undo)
(global-set-key [f3]		'isearch-repeat-forward)
(global-set-key [(shift f3)]    'isearch-repeat-backward)
(global-set-key [XF86_Switch_VT_3] 'isearch-repeat-backward)
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
(global-set-key [f9]		'my-isearch-word-forward)
(global-set-key [(shift f9)]    'my-toggle-case-search)
(global-set-key [XF86_Switch_VT_9] 'my-toggle-case-search)
(global-set-key [f10]		'xref-find-definitions)
(global-set-key "\M-."		'xref-find-definitions)
(global-set-key [(shift f10)]   'pop-tag-mark)
(global-set-key [XF86_Switch_VT_10] 'pop-tag-mark)
(global-set-key [f20] 'pop-tag-mark)
(global-set-key [(control f10)] 'xref-find-references)
;; I keep f11 free for temporary bindings
(global-set-key [(shift f11)] 'my-show-messages)
(global-set-key [XF86_Switch_VT_11] 'my-show-messages)
(global-set-key [f12]		'revert-buffer)
(global-set-key [(shift f12)]	'lxr-next-defined)
(global-set-key [XF86_Switch_VT_12] 'lxr-next-defined)
(global-set-key [(control f12)] 'lxr-defined-at-point)

(global-set-key "\C-cd" 'dup-line)
(global-set-key "\C-ce" 'errno-string)
(global-set-key "\C-cg" 'git-diff)
(global-set-key "\C-ck" 'browse-kill-ring)
(global-set-key "\C-co" 'ogrok)

(global-set-key "\C-c8" '80-scan)
(global-set-key "\C-c9" '80-cleanup) ;; shift-8 and ctrl-8 did not work

(global-set-key [(meta right)] 'forward-sexp)
(global-set-key [(meta left)]  'backward-sexp)

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
(global-set-key [button8] 'yank)
(global-set-key [button9] 'kill-region)

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

;;{{{ Packages

;;; -------------------------------------------------------------------------
;;; Some edit-utils packages
(my-feature-cond
  (xemacs
   (iswitchb-default-keybindings))
;;;  (icomplete-mode
;;;   (icomplete-mode 1))
;;;  (ido-mode
;;;   (ido-mode 1))
  (t
   ;; We have a local copy of iswitchb to get around the deprecated message.
   (load (concat dot-dir "misc/iswitchb"))
   (iswitchb-mode 1)))

(show-paren-mode t)

(global-set-key "\C-x\C-b" (global-key-binding "\C-xb"))

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;;; -------------------------------------------------------------------------
;;; Some text-modes packages
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Flyspell
(if (or (locate-file "hunspell" exec-path exec-suffixes 'executable)
	(locate-file "aspell"   exec-path exec-suffixes 'executable)
	(locate-file "ispell"   exec-path exec-suffixes 'executable))
    (progn
      (add-hook 'c-mode-common-hook 'flyspell-prog-mode)
      (add-hook 'lisp-mode-hook 'flyspell-prog-mode)
      (add-hook 'text-mode-hook 'flyspell-mode))
  (add-to-list 'would-have-liked-list 'spell))

;; (when (fboundp 'whitespace-global-mode) (whitespace-global-mode))

;;; -------------------------------------------------------------------------
;; The auto-save.el and backup.el packages collect files in one place
;; I added the following to my crontab:
;; 13 5 * * * find $HOME/.backup -mtime +7 -delete
;; 17 5 * * * find $HOME/.autosave -mtime +7 -delete

(setq auto-save-file-name-transforms `((".*" "~/.autosave/" t)))
(setq backup-directory-alist '((".*" . "~/.backup")))

;;; ----------------------------------------------
;; ws-trim-mode
(when (fboundp 'global-ws-trim-mode)
  (global-ws-trim-mode t)
  (setq ws-trim-mode-line-string nil)
  (set-default 'ws-trim-level 1))

;;; ------------------------------------------------------------
;; Start the server program
(unless (or noninteractive (string= (user-login-name) "root"))
  (server-start))

;;}}}

;;{{{ Optional Init files

;;; ------------------------------------------------------------
;; Some non-standard init files. Start them last so they can override defaults.

;; Ok, this is actually standard...
(setq custom-file (concat dot-dir "custom.el"))
(load custom-file t)

;; Load a file called `system-type' if it exists. The symbol is
;; sanitized so gnu/linux becomes gnu-linux.
(load (replace-regexp-in-string "/" "-" (symbol-name system-type)) t)

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

;; Not sure why Emacs wipes the message in console mode.
(unless noninteractive
  ;; Every time you turn around Emacs is displaying yet another
  ;; stupid^h^h^h^h^h useful message that overwrites my nice friendly
  ;; one. So use a timer to get past them.
  (run-at-time .2 nil 'friendly-message t))

;;}}}

;; end of .emacs "May the `(' be with `)'"
