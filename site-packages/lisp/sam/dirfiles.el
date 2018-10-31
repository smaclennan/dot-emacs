(require 'sam-common)

(my-feature-cond
  (xemacs
   (defun directory-files-recursive (dir &optional match)
     "Return a list of files in DIR recursively descending all
subdirectories that do not start in a dot (.). If MATCH is non-nil,
match all files against the regular expression."
     (let ((files (directory-files dir t match nil t)))
       (dolist (d (directory-files dir nil "^[^.].*" nil 'dirs))
	 (setq files (append files (directory-files-recursive (concat dir "/" d) match))))
       files))

   (defun insert-file-contents-safe (file)
	"XEmacs `insert-file-contents' cannot handle a zero length file"
	(when (> (nth 7 (file-attributes file)) 0)
	  (insert-file-contents file)))

   (defun subdirs (dir)
     "Return a list of sub-directories in DIR. All dot directories
are ignored. The list is not sorted."
     (directory-files dir t "^[^.]" t 'dirs))

  ) ;; xemacs
  (t
   (defun directory-files-recursive-intern (dir &optional match)
     "Internal function for `directory-files-recursive'."
     (let (files)
       (dolist (d (directory-files dir t "^[^.].*"))
	 (if (file-directory-p d)
	     (setq files (append files (directory-files-recursive-intern d match)))
	   (when (or (not match) (string-match match d))
	     (setq files (append files (list d))))))
       files))

   (defun directory-files-recursive (dir &optional match)
     "Return a list of files in DIR recursively descending all
subdirectories that do not start in a dot (.). If MATCH is non-nil,
match all files against the regular expression."
     (when match (setq match (concat "/" match)))
     (directory-files-recursive-intern dir match))

   (defsubst insert-file-contents-safe (file) (insert-file-contents file))

   (defun subdirs (dir)
     "Return a list of sub-directories in DIR. All dot directories
are ignored. The list is not sorted."
  (let (dirlist)
    (dolist (subdir (directory-files dir t "^[^.]" t))
      (when (file-directory-p subdir)
	(setq dirlist (append dirlist (list subdir)))))
    dirlist))
   ))

(provide 'dirfiles)
