;;; sam-common.el --- SAM's Common macros/functions

;; Copyright (C) 2011 Sean MacLennan

(defmacro emacs>= (major minor)
  `(or (> emacs-major-version ,major)
      (and (eq emacs-major-version ,major)
	   (>= emacs-minor-version ,minor))))

;;;###autoload
(defmacro my-feature-cond (&rest clauses)
  "Test CLAUSES for feature, function, or variable at compile time.
Each clause is (FEATURE BODY...)."
  (cl-block nil
    (dolist (x clauses)
      (let ((feature (car x)))
	(when (or (eq feature t)
		  (featurep feature)
		  (fboundp feature)
		  (boundp feature))
	  (cl-return (cons 'progn (cdr x))))))))

;; Earlier Emacs must require cl
(unless (emacs>= 25 1) (require 'cl))

;;;###autoload
(defmacro my-interactive-p () `(called-interactively-p 'interactive))

;;;###autoload
(defmacro event-point (event) `(cl-cadadr event))

;;;###autoload
(defun push-tag-mark ()
  (eval-and-compile
    (if (emacs>= 25 1)
	(require 'xref)
      (require 'ring)
      (require 'etags)))
  (my-feature-cond
    (xref-push-marker-stack (xref-push-marker-stack))
    (t (ring-insert find-tag-marker-ring (point-marker))))) ;; < 25.1

(provide 'sam-common)
