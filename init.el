;; Emacs initialization. Should work with Emacs >= 24.

; This is the one key binding I must have... switch ASAP
(global-set-key "\C-x\C-b" 'switch-to-buffer)

(dolist (dir '("lisp" "misc"))
  (add-to-list 'load-path (concat user-emacs-directory dir))
  (load (concat dir "-loaddefs") t t))

;; This is when Emacs went insane about common lisp
(if (< emacs-major-version 25) (load (concat user-emacs-directory "compat")))

;; The user-init file allows for user/machine specific
;; initialization. It must be very early for variables like
;; `laptop-mode' to work. Use `after-init-hook' if you need to clean
;; something up at the end.
(load (concat user-emacs-directory "user-init") t)

(rcfiles-register-rc-files)

(if window-system
    (load (concat rcfiles-directory "/window-config"))
  (menu-bar-mode -1)) ;; Yes, Emacs has a menu bar in console mode

(setq track-eol t
      kill-whole-line t
      next-line-add-newlines nil
      inhibit-default-init t
      inhibit-startup-message t
      initial-scratch-message ";; This buffer is for goofing around in.\n\n"
      visible-bell t
      extended-command-suggest-shorter nil
      inhibit-startup-echo-area-message (user-login-name))

(put 'narrow-to-region 'disabled nil) ;; Why? Just why?
(fset 'yes-or-no-p 'y-or-n-p)

;; Let's try making _ part of a "word". C & C++ done in cc-mode-rc.el
(modify-syntax-entry ?_ "w" (standard-syntax-table))

;; There are going to be large files. e.g. tags files
(setq large-file-warning-threshold #x3000000) ;; 48M

;;; ------------------------------------------------------------
;;; Keys

;;;; Function keys.
;; Shifted function keys: [(shift f1)] == [XF86_Switch_VT_1]
(global-set-key [f1]            'find-file)
(global-set-key [f2]		'undo)
(global-set-key [f3]		'isearch-repeat-forward)
(global-set-key [(shift f3)]    'isearch-repeat-backward)
(global-set-key [f4]		'next-error)
(global-set-key [f5]		'query-replace)
(global-set-key [(shift f5)]    'query-replace-regexp)
(global-set-key [f6]		'git-grep-at-point)
(global-set-key [(shift f6)]	'git-grep)
(global-set-key [(control f6)]	'git-grep-toggle-top-of-tree)
(global-set-key [f7]		'compile)
(global-set-key [(shift f7)]    'my-make-clean)
(global-set-key [(control f7)]	'my-set-compile)
(global-set-key [f8] 'my-grep)
(global-set-key [(shift f8)] 'my-grep-find)
(global-set-key [(control f8)]	'my-checkpatch)
(global-set-key [f9]		'my-isearch-word-forward)
(global-set-key [(shift f9)]    'my-toggle-case-search)
(global-set-key [f10]		'xref-find-definitions)
(global-set-key [(shift f10)]   'pop-tag-mark)
(global-set-key [(control f10)] 'xref-find-references)
(global-set-key [f11] nil) ;; I keep f11 free for temporary bindings
(global-set-key [(shift f11)] 'my-show-messages)
(global-set-key [f12]		'revert-buffer)
(global-set-key [(shift f12)]	'lxr-next-defined)
(global-set-key [(control f12)] 'lxr-defined-at-point)

(global-set-key "\M-."		'xref-find-definitions-prompt)
(global-set-key [(meta right)] 'forward-sexp)
(global-set-key [(meta left)]  'backward-sexp)

(global-set-key "\C-cd" 'dup-line)
(global-set-key "\C-ce" 'errno-string)
(global-set-key "\C-cg" 'git-diff)
(global-set-key "\C-ci" 'tag-includes)
(global-set-key "\C-co" 'ogrok)

(global-set-key "\C-c8" '80-scan)
(global-set-key "\C-c9" '80-cleanup) ;; shift-8 and ctrl-8 did not work

;; For some reason this doesn't have a key binding
(global-set-key "\C-hz" 'apropos-variable)

;;; ---- GNU Emacs really really needs a signal-error-on-buffer-boundary

;; Using defadvice for these functions breaks minibuffer history
(defun my-previous-line (arg)
  "`previous-line' with no signal on end-of-buffer."
  (interactive "p")
  (condition-case nil
      (with-no-warnings ;; Yes, I want the interactive version
	(previous-line arg))
    (beginning-of-buffer)))

(defun my-next-line (arg)
  "`previous-line' with no signal on end-of-buffer."
  (interactive "p")
  (condition-case nil
      (with-no-warnings ;; Yes, I want the interactive version
	(next-line arg))
    (end-of-buffer)))

(global-set-key (kbd "<up>") 'my-previous-line)
(global-set-key (kbd "<down>") 'my-next-line)
(global-set-key "\C-p" 'my-previous-line)
(global-set-key "\C-n" 'my-next-line)

(defadvice scroll-down (around my-scroll-down activate)
  "`scroll-down' with no signal on end-of-buffer."
  (condition-case nil
      ad-do-it
    (beginning-of-buffer)))

(defadvice scroll-up (around my-scroll-up activate)
  "`scroll-up' with no signal on end-of-buffer."
  (condition-case nil
      ad-do-it
    (end-of-buffer)))

;;; ----

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

(defun my-toggle-case-search ()
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (message "Case sensitive search %s." (if case-fold-search "off" "on")))

;; Tilt wheel on Logitech M500 + others
;;(global-set-key [button6] ')
;;(global-set-key [button7] ')

;; Side buttons on Logitech M500 + others
(global-set-key [button8] 'yank)
(global-set-key [button9] 'kill-region)

(global-set-key "\C-x\C-l"	'list-buffers)
(global-set-key "\C-x\C-k"	'kill-buffer)
(global-set-key "\C-xw"	'what-line)
(global-set-key "\M-#"		'my-calc)
(global-set-key [(iso-left-tab)] 'tab-to-tab-stop)

;;; ------------------------------------------------------------
;;; Packages

(defvar commit-names '("COMMIT_EDITMSG" "svn-commit.tmp" "README")
  "* List of commit buffer names.")

(defun check-for-commit ()
  "If this is a commit buffer, set to text mode."
  (when (eq major-mode 'fundamental-mode)
    (let ((buff (buffer-name)))
      (dolist (name commit-names)
	(when (string= buff name)
	  (text-mode))))))
(add-hook 'find-file-hooks 'check-for-commit t)

(iswitchb-mode 1)
(delete-selection-mode)

(global-set-key "\C-x\C-b" (global-key-binding "\C-xb"))

;; The auto-save.el and backup.el packages collect files in one place
;; I added the following to my crontab:
;; 13 5 * * * find $HOME/.backup -mtime +7 -delete
;; 17 5 * * * find $HOME/.autosave -mtime +7 -delete

(setq auto-save-list-file-prefix nil) ;; don't create auto-save-list directory
(setq auto-save-file-name-transforms `((".*" "~/.autosave/" t)))
(setq backup-directory-alist '((".*" . "~/.backup")))

(unless noninteractive (server-start))

;;; ------------------------------------------------------------
;;; Optional Init files

;; Load a file called sys/`system-type' if it exists.
(load (concat user-emacs-directory "sys/" (symbol-name system-type)) t)

(load (concat user-emacs-directory "work") t)

;;; ------------------------------------------------------------
;;; Final results

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
  ;; stupid^h^h^h^h^h "useful" message that overwrites my nice friendly
  ;; one. So use a timer to get past them.
  (run-at-time .25 nil 'friendly-message))

;; end of .emacs "May the `(' be with `)'"
