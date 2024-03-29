;; Uses 24: Ubuntu 16.04

;; Back before GNU Emacs went insane about common lisp
(require 'cl)

(provide 'cl-extra)
(provide 'cl-macs)

(load "compat-26")

;; not enabled in 24
(electric-indent-mode)

;; 24.2 needs this... 24.4 does not
(unless (fboundp 'cl-remove-if)
  (defalias 'cl-remove-if 'remove-if)
  (defalias 'cl-loop 'loop)
  (defalias 'cl-block 'block)
  (defalias 'cl-return 'return)
  (defalias 'cl-caddr 'caddr)
  (defalias 'cl-cdadr 'cdadr)
  (defalias 'cl-cadadr 'cadadr)
  (defalias 'cl-position 'position)
  (defalias 'cl-rotatef 'rotatef)
  )

;; xref compatibility functions

(defun xref-find-definitions (identifier)
  "Find tag (in current tags table) whose name contains IDENTIFIER.

This is not a correct implementation of xref-find-definitions. If
there are multiple definitions it always goes to the most exact
definition. I provide a `find-tag-next' to go to the next
definition."
  (interactive
   (list (let ((word (current-word)))
	   (if (or current-prefix-arg (not word))
	       (read-string (format "Identifier [%s]: " word) nil nil word)
	     word))))
  (find-tag identifier))

(defun find-tag-next ()
  (interactive)
  (find-tag last-tag t))

(defun xref-find-references (indentifier)
  (interactive)
  (error "Not supported"))

(defun xref-push-marker-stack ()
  (interactive)
  (ring-insert find-tag-marker-ring (point-marker)))

(defvar lisp-el-font-lock-keywords-2 nil)

(defun directory-files-recursively (dir regexp)
  "Basically a 'find DIR -name REGEXP -print'."
  (let (files path)
    (dolist (file (directory-files-and-attributes dir t "^[^.].*"))
      (setq path (car file))
      (if (eq (cadr file) t)
	  ;; directory
	  (setq files (append files (directory-files-recursively path regexp)))
	;; file
	(if (string-match regexp path)
	    (setq files (cons path files)))))
    files))

;;;###autoload
(defmacro setq-local (var val)
  `(set (make-variable-buffer-local ,var) ,val))
