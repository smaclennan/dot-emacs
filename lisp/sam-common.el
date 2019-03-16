;;; sam-common.el --- SAM's Common macros

;; Copyright (C) 2011-2019 Sean MacLennan

;;;###autoload
(defmacro emacs>= (major minor)
  `(or (> emacs-major-version ,major)
       (and (eq emacs-major-version ,major)
	    (>= emacs-minor-version ,minor))))

;;;###autoload
(defmacro my-feature-cond (&rest clauses)
  "Test CLAUSES for feature, function, or variable at compile time.
Each clause is (FEATURE BODY...)."
  `(cl-block nil
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
(defmacro basename (name)
  `(if (string-match "/\\([^/]+\\)/?$" ,name)
       (match-string 1 ,name)
     ,name))

;; This is almost 4x faster than shell-command-to-string
;;;###autoload
(defun uname (arg)
  (with-temp-buffer
    (call-process "uname" nil t nil arg)
    (buffer-substring (point-min) (1- (point-max)))))

(provide 'sam-common)
