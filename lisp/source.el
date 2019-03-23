;; I cannot make up my mind between the two implementations. Both have
;; pros and cons. So I am going to try both.

;; source-diff()
;; Pros:
;; * Cleaner changes since it is comparing apples to apples.
;; Cons:
;; * Takes 2 setvars for an update.
;; * Relies on diff -u (not a huge problem, but...).

;; source-internal()
;; Pros:
;; * More lispish, less shell commands ;)
;; * One setvar per update.
;; * About twice as fast... although we are looking at ms per call.
;; * Could be optimized more... but probably not worth it.
;; Cons:
;; * Updates more variables the first time since Emacs does not seem
;;   to exactly match shell. This is the one I am worried about.

(defvar source-func 'source-internal
  "Source function to use.")

(defun source-setenv (str var val)
  "This is purely a debug helper."
  (message "%s %s = %s" str var val)
  (setenv var val)
  )

(defun source-rmenv (str var val)
  "This is purely a debug helper."
  (message "%s %s = %s" str var val)
  (setenv var)
  )

;; Based on idea from https://gist.github.com/ffevotte/9345586
;;;###autoload
(defun source-diff (filename)
  "Update environment variables from a shell script."
  (interactive "fSource: ")
  (message "Sourcing %s..." filename)
  (let ((tmpfile (make-temp-file "source")))
    (with-temp-buffer
      (shell-command (concat "source " filename ";env") t)
      (write-region nil nil tmpfile)
      (erase-buffer)
      (shell-command (concat "env | diff -u - " tmpfile) t)
      (while (search-forward-regexp "^\\([-+]\\)\\([A-Z][^=]*\\)=\\(.*\\)" nil t)
	(let ((var (match-string 2)) (val (match-string 3)))
	  (if (string= (match-string 1) "-")
	      (source-rmenv "Remove" var val)
	    (source-setenv "Update" var val)))))
    (delete-file tmpfile))
  (message "Sourcing %s...done." filename))

;;;###autoload
(defun source-internal (filename)
  "Update environment variables from a shell script."
  (interactive "fSource: ")
  (message "Sourcing %s..." filename)
  (let ((envs (mapcar (lambda (one)
			(string-match "^\\([^=]+\\)=?\\(.*\\)" one)
			(list (match-string 1 one) (match-string 2 one)))
		      process-environment)))
    (with-temp-buffer
      (shell-command (concat "source " filename ";env") t)
      (while (re-search-forward "^\\([^=]+\\)=\\(.*\\)" nil t)
	(let* ((var (match-string 1)) (val (match-string 2))
	       (env (assoc var envs)))
	  (if env
	      (progn
		(unless (or (string= (cadr env) val) (string= var "_"))
		  (source-setenv "Update" var val))
		(setq env (delq env envs)))
	    (source-setenv "New" var val)))))
    (dolist (rm envs)
      (source-rmenv "Remove" (car rm) (cadr rm))))
  (message "Sourcing %s...done." filename))

;;;###autoload
(defun source (filename)
  "Update environment variables from a shell script."
  (interactive "fSource: ")
  (apply source-func filename nil))
