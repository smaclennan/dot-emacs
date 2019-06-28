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

(defun directory-dirs-recursive-intern (dir)
  "Internal function for `directory-files-recursive'."
  (let (dirs)
    (dolist (d (directory-files dir t "^[^.].*"))
      (when (file-directory-p d)
	(setq dirs (append dirs (list d)))
	(setq dirs (append dirs (directory-dirs-recursive-intern d)))))
    dirs))

;;;###autoload
(defun directory-dirs-recursive (dir)
  "Return a list of directories in DIR recursively descending all
subdirectories that do not start in a dot (.)."
  (directory-dirs-recursive-intern dir))

;;;###autoload
(defun subdirs (dir &optional nosort)
  "Return a list of sub-directories in DIR. All dot directories
are ignored. If NOSORT is non-nil then the list is not sorted."
  (let (dirlist)
    ;; Note: directory-files-and-attributes
    (dolist (subdir (directory-files dir t "^[^.].*" nosort))
      (when (file-directory-p subdir)
	(setq dirlist (append dirlist (list subdir)))))
    dirlist))

(provide 'dirfiles)
