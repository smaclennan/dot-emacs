;; -*- lexical-binding: t -*-
;;; diff-kconfig.el - A vain attempt to make diffing kernel config files easier
;;; Copyright (C) 2016 Sean MacLennan

(defvar dkc-no-arch t
  "*Remove ARCH entries.")

(defvar dkc-set-or-not t
  "*Converts all modules to yes.")

(defun dkc-preprocess (config buff)
  (message "Processing %s..." config)
  (switch-to-buffer buff)
  (insert-file-contents config nil nil nil t)

  (when nil
  ;; Remove comment char from not set lines
  ;; Delete normal comments
  ;; Delete empty lines
  (goto-char (point-min))
  (while (and (re-search-forward "^\\(#\\|$\\)" nil t) (not (eobp)))
    (if (looking-at " CONFIG")
	(progn
	  (beginning-of-line)
	  (delete-char 2))
      (kill-whole-line)))
  )

  ;; Remove comment lines
  (goto-char (point-min))
  (while (re-search-forward "^#.*$" nil t)
    (replace-match ""))

  (trim-empty-lines)

  (when dkc-no-arch
    (goto-char (point-min))
    (while (re-search-forward "^CONFIG_ARCH_" nil t)
      (kill-whole-line)))

  (when dkc-set-or-not
    (goto-char (point-min))
    (while (re-search-forward "=m$" nil t)
      (replace-match "=y")))

  (sort-lines nil (point-min) (point-max))

  (set-buffer-modified-p nil)
  )

;;;###autoload
(defun diff-kconfig (c1 c2)
  (interactive "fConfig1: \nfConfig2: ")
  (let ((buf1 (get-buffer-create "*kconfig1*"))
	(buf2 (get-buffer-create "*kconfig2*")))
    (save-excursion
      (dkc-preprocess c1 buf1)
      (dkc-preprocess c2 buf2)
      )
    (ediff-buffers buf1 buf2)
  ))

(require 'trim)
