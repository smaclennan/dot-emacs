;; Emacs initialization. Should work with Emacs >= 24.

;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html

; This is the one key binding I must have... switch ASAP
(global-set-key "\C-x\C-b" 'switch-to-buffer)

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(load "lisp-loaddefs" t t)
(add-to-list 'load-path (concat user-emacs-directory "sys"))
(load "sys-loaddefs" t t)
(load (format "compat-%d" emacs-major-version) t)
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; The user-init file allows for user/machine specific
;; initialization. It must be very early for variables like
;; `laptop-mode' to work. Use `after-init-hook' if you need to clean
;; something up at the end.
(load (concat user-emacs-directory "user-init") t)

;; Load a file called sys/`system-type' if it exists.
;; Must be after user-init.el but very early
(load (replace-regexp-in-string "gnu/" "" (symbol-name system-type)) t)

;; This must come before configurations of installed packages.
(if (file-exists-p (concat user-emacs-directory "elpa"))
    (package-initialize)
  (defun package-installed-p (pkg) nil))

(rcfiles-register-rc-files)

(load (concat rcfiles-directory
	      (if window-system "/window" "/console") "-config"))

(setq track-eol t
      kill-whole-line t
      visible-bell t
      extended-command-suggest-shorter nil)

(put 'narrow-to-region 'disabled nil) ;; Why? Just why?
(fset 'yes-or-no-p 'y-or-n-p)

;; Let's try making _ part of a "word". C & C++ done in cc-mode-rc.el
(modify-syntax-entry ?_ "w" (standard-syntax-table))

;; There are going to be large tag files
(setq large-file-warning-threshold #x4800000) ;; 72M

(iswitchb-mode)
(global-set-key "\C-x\C-b" (global-key-binding "\C-xb"))

(delete-selection-mode)

;;; ------------------------------------------------------------
;;; Keys

;;;; Function keys.
;; Shifted function keys: [(shift f1)] == [XF86_Switch_VT_1]
(global-set-key [f1]            'find-file)
(global-set-key [(shift f1)]    'update-local-compile-command)
(global-set-key [(control f1)]  'add-local-compile-command)
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
(global-set-key [f8]		'my-grep-i-feel-lucky)
(global-set-key [(shift f8)]	'my-grep)
(global-set-key [(control f8)]	'my-grep-find)
(global-set-key [f9]		'my-isearch-word-forward)
(global-set-key [(shift f9)]    'my-toggle-case-search)
(global-set-key [(control f9)]	'my-checkpatch)
(global-set-key [f10]		'xref-find-definitions)
(global-set-key [(shift f10)]   'pop-tag-mark)
(global-set-key [(control f10)] 'xref-find-references)
(global-set-key [f11] nil) ;; I keep f11 free for temporary bindings
(global-set-key [(shift f11)]	'my-show-messages)
(global-set-key [f12]		'revert-buffer)
(global-set-key [(shift f12)]	'lxr-next-defined)
(global-set-key [(control f12)] 'lxr-defined-at-point)

(global-set-key "\M-."		'xref-find-definitions-prompt)
(global-set-key [(meta right)]	'forward-sexp)
(global-set-key [(meta left)]	'backward-sexp)


(global-set-key "\C-x\C-l"	'list-buffers)
(global-set-key "\C-x\C-k"	'kill-buffer)
(global-set-key "\C-xw"		'what-line)
(global-set-key "\M-#"		'my-calc)
(global-set-key [(iso-left-tab)] 'tab-to-tab-stop)

(global-set-key "\C-cd" 'dup-line)
(global-set-key "\C-ce" 'errno-string)
(global-set-key "\C-cg" 'git-diff)
(global-set-key "\C-ci" 'tag-includes)
(global-set-key "\C-co" 'ogrok)

(global-set-key "\C-c\C-t" 'swap-in-word)

;; For some reason this doesn't have a key binding
(global-set-key "\C-hz" 'apropos-variable)

;; Using defadvice for these functions breaks minibuffer history
(defun my-previous-line (arg)
  "`previous-line' with no signal on beginning-of-buffer."
  (interactive "p")
  (line-move (- arg) t))

(defun my-next-line (arg)
  "`next-line' with no signal on end-of-buffer."
  (interactive "p")
  (line-move arg t))

(global-set-key [up]   'my-previous-line)
(global-set-key [down] 'my-next-line)
(global-set-key "\C-p" 'my-previous-line)
(global-set-key "\C-n" 'my-next-line)

(defadvice scroll-down (around my-scroll-down activate)
  "`scroll-down' with no signal on end-of-buffer."
  (condition-case nil
      ad-do-it
    (error (goto-char (point-min)))))

(defadvice scroll-up (around my-scroll-up activate)
  "`scroll-up' with no signal on end-of-buffer."
  (condition-case nil
      ad-do-it
    (error (goto-char (point-max)))))

(defun my-show-messages ()
  "Show messages in other window."
  (interactive)
  (switch-to-buffer-other-window "*Messages*"))

;; end of .emacs "May the `(' be with `)'"
