;; Emacs initialization. Should work with Emacs >= 24.
;;https://www.gnu.org/software/emacs/manual/html_node/elisp/Startup-Summary.html

; This is the one key binding I must have... switch ASAP
(global-set-key "\C-x\C-b" 'switch-to-buffer)

;; Rather than duplicating all the code here... just load batch-helper.el
(load (concat user-emacs-directory "lisp/batch-helper"))

;; The user-init file allows for user/machine specific
;; initialization. It must be very early for variables like
;; `laptop-mode' to work. Use `after-init-hook' if you need to clean
;; something up at the end.
(load (concat user-emacs-directory "user-init") t)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; This must come before configurations of installed packages.
(if (file-exists-p (concat user-emacs-directory "elpa"))
    (package-initialize)
  (defun package-installed-p (pkg) nil))

(rcfiles-register-rc-files)

(load (concat rcfiles-directory
	      (if window-system "/window" "/console") "-config"))

(put 'narrow-to-region 'disabled nil) ;; Why? Just why?
(fset 'yes-or-no-p 'y-or-n-p)

;; This gets rid of the iswitchb deprecated message by moving it out
;; of the obsolete directory.
(let ((to (concat user-emacs-directory "lisp/iswitchb.elc")))
  (unless (file-exists-p to)
    (make-symbolic-link (locate-library "iswitchb") to t)))

(iswitchb-mode)
(global-set-key "\C-x\C-b" (global-key-binding "\C-xb"))

(delete-selection-mode)

;; end of .emacs "May the `(' be with `)'"
