;;; sam-common.el --- SAM's Common macros/functions

;; Copyright (C) 2011 Sean MacLennan

(dolist (dir '("lisp" "misc"))
  (add-to-list 'load-path (concat user-emacs-directory dir))
  (load (concat dir "-loaddefs") t t))

;; I don't know why the hate against common-lisp
(setq byte-compile-warnings '(not cl-functions))
(require 'cl)
(require 'cl-extra)
(require 'xref)

(defmacro my-feature-cond (&rest clauses)
  "Test CLAUSES for feature, function, or variable at compile time.
Each clause is (FEATURE BODY...)."
  (dolist (x clauses)
    (let ((feature (car x))
	  (body (cdr x)))
      (when (or (eq feature t)
		(featurep feature)
		(fboundp feature)
		(boundp feature))
	(return (cons 'progn body))))))
(put 'my-feature-cond 'lisp-indent-hook 'defun)

;; Must be a macro to work
(defmacro my-interactive-p () `(called-interactively-p 'interactive))

(defun event-point (event) (nth 1 (event-start event)))

(defun push-tag-mark ()
  (my-feature-cond
    (xref-push-marker-stack (xref-push-marker-stack))
    (t (ring-insert find-tag-marker-ring (point-marker)))))

(provide 'sam-common)
