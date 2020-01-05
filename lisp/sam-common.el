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

;;;###autoload
(defmacro measure-time (loops &rest body)
  "Measure the time it takes to evaluate BODY LOOPS times."
  `(let ((time (current-time)))
     (cl-loop repeat ,loops do ,@body)
     (message "%.06f" (float-time (time-since time)))))

;; This is almost 4x faster than shell-command-to-string
;;;###autoload
(defmacro uname (arg)
  `(with-temp-buffer
    (call-process "uname" nil t nil ,arg)
    (buffer-substring (point-min) (1- (point-max)))))

;; For some reason this needs to be a function for arm Linux
;;;###autoload
(defun strtol (str)
  "Mimic strtol(str, NULL, 0)... but not exactly"
  (if (string-match "^0[xX]\\(.*\\)" str)
      (string-to-number (match-string 1 str) 16)
    (string-to-number str)))

(provide 'sam-common)
