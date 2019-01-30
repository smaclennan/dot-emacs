;; This is meant to be used to compile lisp files in batch mode.

(dolist (dir '("lisp" "misc"))
  (add-to-list 'load-path (concat user-emacs-directory dir))
  (load (concat dir "-loaddefs") t t))

(defun basename (name)
  (if (string-match "/\\([^/]+\\)/?$" name)
      (match-string 1 name)
    name))

(unless (equal user-emacs-directory default-directory)
  ;; This expects one .el and updates the autoload file
  (let (file)
    (dolist (one command-line-args-left)
      (when (string-match "^[a-zA-Z0-9._-]+\\.el$" one)
	(when file (message "Warning: more than one .el file"))
	(setq file one)))

    (if file
	;; Emacs 23.1 does not support outfile arg to `update-file-autoloads'
	(let ((generated-autoload-file
	       (concat default-directory (basename default-directory) "-loaddefs.el")))
	  (update-file-autoloads file t))
      (message "Warning: No lisp file found"))))
