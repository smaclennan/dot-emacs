(defvar source-exclude-list nil
  "List of environment variables to ignore.")

(defun source-setenv (str var val new)
  "Helper program for nice messages."
  (message "%s %s = %s" str var val)
  (setenv var new))

;;;###autoload
(defun source (filename)
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
	  (unless (member var source-exclude-list)
	    (if env
		(progn
		  (unless (string= (cadr env) val)
		    (source-setenv "Update" var val val))
		  (setq envs (delq env envs)))
	      (source-setenv "New" var val val))))))
    (dolist (rm envs)
      (unless (member (car rm) source-exclude-list)
	(source-setenv "Remove" (car rm) (cadr rm) nil))))
  (message "Sourcing %s...done." filename))

(provide 'source)
