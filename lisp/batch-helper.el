;; -*- lexical-binding: t -*-
;; This is used to compile lisp files in batch mode.

(defvar sys-type (replace-regexp-in-string "gnu/" "" (symbol-name system-type))
  "Simplified version of `system-type'.")

(add-to-list 'load-path (concat user-emacs-directory "lisp"))
(add-to-list 'load-path (concat user-emacs-directory "sys"))

(require 'sam-common)

(load (format "compat-%d" emacs-major-version) t noninteractive)
(load sys-type nil noninteractive) ;; should always exist

;; SAM FIXME
(defun would-like (pkg)
  "Helper for building rc files. Some modes do not have a provide."
  (condition-case nil (require pkg) (error nil)))
