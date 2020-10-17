(defun make-directory-autoloads (dir output-file)
  (let ((generated-autoload-file output-file))
    (update-directory-autoloads dir)))
