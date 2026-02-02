;; -*- lexical-binding: t -*-
;; This is used to compile lisp files in batch mode.

;; SAM FIXME

(defvar sys-type (replace-regexp-in-string "gnu/" "" (symbol-name system-type))
  "Simplified version of `system-type'.")

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "sys"))

(load (format "compat-%d" emacs-major-version) t noninteractive)
(load sys-type nil noninteractive) ;; should always exist

(defun update-loadfile ()
  "This is meant to be called in batch mode. You must specify the loadfile."
  (loaddefs-generate default-directory
  		     (expand-file-name (car command-line-args-left))))

(defun update-sys ()
  (let ((exclude (directory-files "." nil ".*\\.el")))
    (setq exclude (delete "sys-common.el" exclude))
    (setq exclude (delete (concat sys-type ".el") exclude))
    (loaddefs-generate default-directory (expand-file-name "sys-loaddefs.el") exclude)))

(defun would-like (pkg)
  "Helper for building rc files. Some modes do not have a provide."
  (condition-case nil (require pkg) (error nil)))
