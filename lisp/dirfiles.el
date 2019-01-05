(defun directory-files-recursive-intern (dir &optional match)
  "Internal function for `directory-files-recursive'."
  (let (files)
    (dolist (d (directory-files dir t "^[^.].*"))
      (if (file-directory-p d)
	  (setq files (append files (directory-files-recursive-intern d match)))
	(when (or (not match) (string-match match d))
	  (setq files (append files (list d))))))
    files))

;;;###autoload
(defun directory-files-recursive (dir &optional match)
  "Return a list of files in DIR recursively descending all
subdirectories that do not start in a dot (.). If MATCH is non-nil,
match all files against the regular expression."
  (when match (setq match (concat "/" match)))
  (directory-files-recursive-intern dir match))

;;;###autoload
(defun subdirs (dir)
  "Return a list of sub-directories in DIR. All dot directories
are ignored. The list is not sorted."
  (let (dirlist)
    (dolist (subdir (directory-files dir t "^[^.]" t))
      (when (file-directory-p subdir)
	(setq dirlist (append dirlist (list subdir)))))
    dirlist))

(provide 'dirfiles)
