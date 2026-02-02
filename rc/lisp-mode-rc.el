;; -*- lexical-binding: t -*-
(defun makefile-exists-p ()
  (or (file-exists-p "Makefile")
      (file-exists-p "makefile")
      (file-exists-p "GNUmakefile")))

(defvar my-byte-compile-buffer-called nil "Never set this")

(defun my-byte-compile-buffer (ignore-makefile)
  "If a makefile exists in the current directory, call make. Always byte
compile and load the current buffer. With a prefix arg do not check
for a local makefile.

If `compilation-ask-about-save' is nil, saves the file without
asking."
  (interactive "P")
  (require 'compile)
  (save-some-buffers (not compilation-ask-about-save))
  (when (and (not ignore-makefile) (makefile-exists-p))
    (let ((my-byte-compile-buffer-called t))
      (compile compile-command)))
  (emacs-lisp-byte-compile-and-load))

;; Redefine compile
(define-key emacs-lisp-mode-map [f7] 'my-byte-compile-buffer)

(defun gen-compilation-name (mode)
  "Rename the compilation buffer if compiling lisp code. This
allows lisp code to be compiled while doing another compile."
  (if my-byte-compile-buffer-called
      "*lisp-compile*"
    ;; This is the default
    (concat "*" (downcase mode) "*")))
(setq compilation-buffer-name-function 'gen-compilation-name)

;; Bold SAM comments
(comment-warn 'emacs-lisp-mode ";+ ?\\<SAM\\>.*")

;; I don't like the warning-face used in GNU Emacs for functions like `error'.
;; However, the keywords are a defconst, so we must work around that by
;; making a copy.
(let ((mine (copy-tree lisp-el-font-lock-keywords-2)) str)
  (dolist (face mine)
    (setq str (car face))
    (and (stringp str)
	 (string-match "\\berror\\b" str)
	 (setf (cl-cdadr face) (list 'font-lock-keyword-face))))
  (setq lisp-el-font-lock-keywords-2 mine))
