;; Emacs setup -*- Mode:emacs-lisp -*-
;; This file should work with Emacs >= 22.1

;;{{{ Configuration variables / functions

; This is the one key binding I must have... switch ASAP
(global-set-key "\C-x\C-b" 'switch-to-buffer)

(dolist (dir '("lisp" "misc"))
  (add-to-list 'load-path (concat user-emacs-directory dir))
  (load (concat dir "-loaddefs") t t))

;; The user-init file allows for user/machine specific
;; initialization. It must be very early for variables like
;; `laptop-mode' to work. Use `after-init-hook' if you need to clean
;; something up at the end.
(load (concat user-emacs-directory "user-init") t)

(rcfiles-register-rc-files)

;;}}}

;;{{{ Basic Customization

(setq track-eol t
      kill-whole-line t
      next-line-add-newlines nil
      inhibit-default-init t
      inhibit-startup-message t
      initial-scratch-message ";; This buffer is for goofing around in.\n\n"
      visible-bell t)

;; I used to like when the suggestions where good, but not when they
;; are just a shortened version of the command.
(setq suggest-key-bindings nil)

;; This has got to be the hardest variable to set. It seems you *must*
;; hardcode the name and it can't be in a compiled file. Yes, the
;; docs say it can, but the eval form does not work.
(cond
 ((equal (user-login-name) "sam")
  (setq inhibit-startup-echo-area-message "sam"))
 ((equal (user-login-name) "smaclennan")
  (setq inhibit-startup-echo-area-message "smaclennan"))
 ((equal (user-login-name) "seanm")
  (setq inhibit-startup-echo-area-message "seanm")))

(put 'narrow-to-region 'disabled nil) ;; Why? Just why?
(fset 'yes-or-no-p 'y-or-n-p)

;; Let's try making _ part of a "word". C & C++ done in cc-mode-rc.el
(modify-syntax-entry ?_ "w" (standard-syntax-table))

;; Windowing System Customization
(if window-system
    (load (concat rcfiles-directory "/window-config"))
  ;; Yes, emacs has a menu bar in console mode
  (if (fboundp 'menu-bar-mode)
      (menu-bar-mode -1)))

;;}}}

;;{{{ Keys

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
(global-set-key [(shift f7)]    'my-make-clean)
(global-set-key [XF86_Switch_VT_7] 'my-make-clean)
(global-set-key [(control f7)]	'my-set-compile)
(global-set-key [f8] 'my-grep)
(global-set-key [(shift f8)] 'my-grep-find)
(global-set-key [(control f8)]	'my-checkpatch)
(global-set-key [f9]		'my-isearch-word-forward)
(global-set-key [(shift f9)]    'my-toggle-case-search)
(global-set-key [XF86_Switch_VT_9] 'my-toggle-case-search)
(global-set-key [f10]		'xref-find-definitions)
(global-set-key "\M-."		'xref-find-definitions-prompt)
(global-set-key [(shift f10)]   'pop-tag-mark)
(global-set-key [XF86_Switch_VT_10] 'pop-tag-mark)
(global-set-key [f20] 'pop-tag-mark)
(global-set-key [(control f10)] 'xref-find-references)
(global-set-key [f11] nil) ;; I keep f11 free for temporary bindings
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

;; For some reason this doesn't have a key binding
(global-set-key "\C-hz" 'apropos-variable)

(defun xref-find-definitions-prompt ()
  "Same as `xref-find-defintions' except it always prompts for
the identifier."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'xref-find-definitions)))

(defun my-show-messages ()
  "Show messages in other window."
  (interactive)
  (switch-to-buffer-other-window "*Messages*"))

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

(require 'intellimouse)

(global-set-key "\C-xw"	'what-line)

(global-set-key "\M-#"		'my-calc)

(global-set-key [(iso-left-tab)] 'tab-to-tab-stop)

;;}}}

;;{{{ Programming Packages

;; Always want font-lock
;; Use require. (turn-on-font-lock) caused no end of grief on my work computers.
(unless noninteractive
  (require 'font-lock)
  (global-font-lock-mode 1)) ;; For 21.x

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
;; (icomplete-mode 1
;; (ido-mode 1))
(require 'iswitchb)
(iswitchb-mode 1)

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
      (add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
      (add-hook 'text-mode-hook 'flyspell-mode))
  (message "WARNING: No spell program found"))

;; (when (fboundp 'whitespace-global-mode) (whitespace-global-mode))

;;; -------------------------------------------------------------------------
;; The auto-save.el and backup.el packages collect files in one place
;; I added the following to my crontab:
;; 13 5 * * * find $HOME/.backup -mtime +7 -delete
;; 17 5 * * * find $HOME/.autosave -mtime +7 -delete

(setq auto-save-list-file-prefix nil) ;; don't create auto-save-list directory
(setq auto-save-file-name-transforms `((".*" "~/.autosave/" t)))
(setq backup-directory-alist '((".*" . "~/.backup")))

;;; ----------------------------------------------
;; whitespace trimming

(require 'ws-butler)
(add-hook 'prog-mode-hook #'ws-butler-mode)
;; (global-ws-trim-mode t)
;; (setq ws-trim-mode-line-string nil)
;; (set-default 'ws-trim-level 1)))

;;; ------------------------------------------------------------
;; Start the server program
(unless noninteractive (server-start))

;;}}}

;;{{{ Optional Init files

;; Load a file called `system-type' if it exists. The symbol is
;; sanitized so gnu/linux becomes gnu-linux.
(load (replace-regexp-in-string "/" "-" (symbol-name system-type)) t)

(load (concat user-emacs-directory "work") t)

;;}}}

;;{{{ Final results

(defun friendly-message ()
  (interactive)
  (let ((hour (nth 2 (decode-time))))
    (message "Good %s %s"
	     (cond ((< hour 12) "morning")
		   ((< hour 18) "afternoon")
		   (t           "evening"))
	     (user-full-name))))

;; Not sure why Emacs wipes the message in console mode.
(unless noninteractive
  ;; Every time you turn around Emacs is displaying yet another
  ;; stupid^h^h^h^h^h useful message that overwrites my nice friendly
  ;; one. So use a timer to get past them.
  (run-at-time .2 nil 'friendly-message))

;;}}}

;; end of .emacs "May the `(' be with `)'"
