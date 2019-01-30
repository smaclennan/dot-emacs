;;; sam-common.el --- SAM's Common macros/functions

;; Copyright (C) 2011 Sean MacLennan

(dolist (dir '("lisp" "misc"))
  (add-to-list 'load-path (concat user-emacs-directory dir))
  (load (concat dir "-loaddefs") t t))

(require 'xref)

;;;###autoload
(defmacro my-feature-cond (&rest clauses)
  "Test CLAUSES for feature, function, or variable at compile time.
Each clause is (FEATURE BODY...)."
  (cl-block nil
    (dolist (x clauses)
      (let ((feature (car x))
	    (body (cdr x)))
	(when (or (eq feature t)
		  (featurep feature)
		  (fboundp feature)
		  (boundp feature))
	  (cl-return (cons 'progn body)))))))

;;;###autoload
(defmacro my-interactive-p () `(called-interactively-p 'interactive))

;;;###autoload
(defmacro event-point (event) `(cadadr event))

;;;###autoload
(defun push-tag-mark ()
  (my-feature-cond
    (xref-push-marker-stack (xref-push-marker-stack))
    (t (ring-insert find-tag-marker-ring (point-marker))))) ;; < 25.1

(provide 'sam-common)
