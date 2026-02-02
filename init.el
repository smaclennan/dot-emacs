;; -*-no-byte-compile: t -*-
;; Emacs initialization. Should work with Emacs >= 24

;; This is the one key binding I must have... switch ASAP
(global-set-key "\C-x\C-b" 'switch-to-buffer)

;; Load backward compatibility and system specific files
(let ((load-path (cons (concat user-emacs-directory "sys") load-path)))
  (load (format "compat-%d" emacs-major-version) t)
  (load (if (eq system-type 'gnu/linux) "linux" (symbol-name system-type))))

(when (< emacs-major-version 31)
  ;; Add our lisp subdir to the load-path and load it
  (let ((dir (concat user-emacs-directory "lisp/")))
    (loaddefs-generate dir (concat dir "my-loaddefs.el"))
    (add-to-list 'load-path dir)
    (load "my-loaddefs" t t)))

;; The user-init file allows for user/machine specific init. It must
;; be very early for variables like `laptop-mode' to work. Use
;; `after-init-hook' if you need to clean something up at the end.
(load (concat user-emacs-directory "user-init") t)

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;; This must come before configurations of installed packages
(if (file-directory-p (concat user-emacs-directory "elpa"))
    (package-initialize)
  (defun package-installed-p (pkg) nil))

;; Everything else is configured through rc files
(rcfiles-register-rc-files)

;; I have to work
(load "~/work/emacs/work-init" t)

;; end of .emacs "May the `(' be with `)'"
