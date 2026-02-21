;; -*- no-byte-compile: t -*-

;; WARNING: This is included in all other compats

(defun deal-with-loaddefs (&optional dir)
  " Before Emacs 31 we need to deal with loaddefs after they are generated."
  (when (< emacs-major-version 31)
    (unless dir (setq dir (file-name-directory load-file-name)))
    (add-to-list 'load-path dir)
    (loaddefs-generate dir (concat dir "my-loaddefs.el"))
    (load "my-loaddefs" t t)))
