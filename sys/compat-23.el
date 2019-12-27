;; WARNING: This is currently just a copy of compat-24.el.

;; Back before GNU Emacs went insane about common lisp
(require 'cl)

(provide 'cl-extra)

;; 24.2 needs this... 24.5 does not
(unless (fboundp 'cl-remove-if)
  (defalias 'cl-remove-if 'remove-if)
  (defalias 'cl-loop 'loop)
  (defalias 'cl-block 'block)
  (defalias 'cl-return 'return)
  (defalias 'cl-caddr 'caddr)
  (defalias 'cl-cdadr 'cdadr)
  (defalias 'cl-cadadr 'cadadr)
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
