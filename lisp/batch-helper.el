;; -*- lexical-binding: t -*-
;; This is used to compile lisp files in batch mode.

(defvar sys-type (replace-regexp-in-string "gnu/" "" (symbol-name system-type))
  "Simplified version of `system-type'.")

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "sys"))

(require 'sam-common)

(load (format "compat-%d" emacs-major-version) t noninteractive)
(load sys-type nil noninteractive) ;; should always exist

;; This is for the rc directory
;;
;; They are meant to be run when the mode is loaded, so everything
;; should be resolved. So do the load here.
(when (string= default-directory (concat user-emacs-directory "rc/"))
  (defun comment-warn (mode &optional re)
    "Helper for making comments matching RE in MODE bold.
If RE is nil, it is assumed the comment character is #."
    (unless re (setq re "# ?\\<SAM\\>.*"))
    (let ((keyword (list (list re 0 (quote `font-lock-comment-warn-face) t))))
      (font-lock-add-keywords mode keyword)))

  (let ((fname (car (last command-line-args-left))) pkg)
    (string-match "\\(.*\\)-rc.el" fname)
    (setq pkg (match-string 1 fname))
    (load pkg t)))
