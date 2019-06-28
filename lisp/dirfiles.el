;;;###autoload
(defun directory-files-recursive (dir &optional match)
  "Return a list of files in DIR recursively descending all
subdirectories that do not start in a dot (.).

If MATCH is non-nil, match all files against the regular
expression.  If you want to match only against the file portion
/<match>$ is recommended."
  (let (files)
    (dolist (d (directory-files-and-attributes dir t "^[^.].*"))
      (if (eq (cadr d) t)
	  (setq files (append files (directory-files-recursive (car d) match)))
	(when (or (not match) (string-match match (car d)))
	  (setq files (append files (list (car d)))))))
    files))

;;;###autoload
(defun directory-dirs-recursive (dir)
  "Return a list of directories in DIR recursively descending all
subdirectories that do not start in a dot (.)."
  (let (dirs)
    (dolist (d (directory-files-and-attributes dir t "^[^.].*"))
      (when (eq (cadr d) t)
	(setq dirs (append dirs (list (car d))
			   (directory-dirs-recursive (car d))))))
    dirs))

;;;###autoload
(defun subdirs (dir &optional nosort)
  "Return a list of sub-directories in DIR. All dot directories
are ignored. If NOSORT is non-nil then the list is not sorted."
  (let (dirlist)
    (dolist (subdir (directory-files-and-attributes dir t "^[^.].*" nosort))
      (when (eq (cadr subdir) t)
	(setq dirlist (append dirlist (list (car subdir))))))
    dirlist))

(provide 'dirfiles)
