;; -*-no-byte-compile: t -*-
;; Emacs initialization. Should work with Emacs >= 24

;; This is the one key binding I must have... switch ASAP
(global-set-key "\C-x\C-b" 'switch-to-buffer)

;; Add our sub-directories to the load-path
(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "sys"))

;; Load backward compatibility and system specific files
(load (format "compat-%d" emacs-major-version) t)
(load (replace-regexp-in-string "gnu/" "" (symbol-name system-type)))

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

;; end of .emacs "May the `(' be with `)'"
