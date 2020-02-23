;; This is used to compile lisp files in batch mode.

(defvar sys-type (replace-regexp-in-string "gnu/" "" (symbol-name system-type))
  "Simplified version of `system-type'.")

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "sys"))

(load (format "compat-%d" emacs-major-version) t noninteractive)
(load sys-type nil noninteractive) ;; should always exist

(eval-when-compile (require 'autoload))

(defun update-loadfile ()
  "This is meant to be called in batch mode. You must specify the loadfile."
  (let ((generated-autoload-file
	 (expand-file-name (car command-line-args-left))))
    (update-directory-autoloads default-directory)))

(defun update-sys ()
  (let ((generated-autoload-file (expand-file-name "sys-loaddefs.el")))
    (update-file-autoloads (concat sys-type ".el") t)))

(defun would-like (pkg)
  "Helper for building rc files. Some modes do not have a provide."
  (condition-case nil (require pkg) (error nil)))
