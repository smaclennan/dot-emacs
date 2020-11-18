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

;;;###autoload
(defun dirfiles (dir &optional match)
  "Return a list of files, and symlinks, from a directory."
  (let (list)
    (dolist (file (directory-files-and-attributes dir nil match))
      (unless (eq (cadr file) t)
	(setq list (nconc list (list (car file))))))
    list))

(provide 'dirdirs)
