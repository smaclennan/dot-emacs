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
(if (file-directory-p (concat user-emacs-directory "elpa"))
    (package-initialize)
  (defun package-installed-p (pkg) nil))

;; Everything else is configured through rc files
(rcfiles-register-rc-files)

;; end of .emacs "May the `(' be with `)'"
