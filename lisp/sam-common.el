;;; sam-common.el --- SAM's Common macros/functions

;; Copyright (C) 2011 Sean MacLennan

;; When building outside emacs dot-dir may not be set
(defvar dot-dir (expand-file-name "~/.emacs.d/"))

(dolist (dir '("lisp" "misc"))
  (add-to-list 'load-path (concat dot-dir dir))
  (load (concat dir "-loaddefs") t t))

;; I don't know why the hate against common-lisp
(setq byte-compile-warnings '(not cl-functions))
(require 'cl)
(require 'cl-extra)
(require 'xref)

;; GNU emacs sets emacs
;; XEmacs sets xemacs
;; SXEmacs sets sxemacs and xemacs
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

(my-feature-cond
  (xemacs ;; Must be a defalias for my-isearch-word-forward
   (defalias 'my-interactive-p 'interactive-p)
   (defalias 'kill-whole-line 'kill-entire-line))
  (t ;; Must be a macro to work
   (defmacro my-interactive-p () `(called-interactively-p 'interactive))

   (defun event-point (event) (nth 1 (event-start event)))

   (defun push-tag-mark ()
     (my-feature-cond
       (xref-push-marker-stack (xref-push-marker-stack))
       (t (ring-insert find-tag-marker-ring (point-marker)))))
   ))

(provide 'sam-common)
