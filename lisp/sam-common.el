;;; sam-common.el --- SAM's Common macros

;; Copyright (C) 2011-2019 Sean MacLennan

;;;###autoload
(defmacro emacs>= (major minor)
  `(or (> emacs-major-version ,major)
      (and (eq emacs-major-version ,major)
	   (>= emacs-minor-version ,minor))))

;; Earlier Emacs must require cl
(unless (emacs>= 25 1) (require 'cl))

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

;;;###autoload
(defmacro my-interactive-p () `(called-interactively-p 'interactive))

;;;###autoload
(defmacro event-point (event) `(cl-cadadr event))

;;;###autoload
(defalias 'push-tag-mark 'xref-push-marker-stack)

;;;###autoload
(defmacro basename (name)
  `(if (string-match "/\\([^/]+\\)/?$" ,name)
      (match-string 1 ,name)
    ,name))

(provide 'sam-common)
