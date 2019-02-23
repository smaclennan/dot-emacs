;; This is meant to be used to compile lisp files in batch mode.

(dolist (dir '("lisp" "misc"))
  (add-to-list 'load-path (concat user-emacs-directory dir))
  (load (concat dir "-loaddefs") t t))

(eval-when-compile (require 'autoload))

(defun update-loadfile ()
  "This is meant to be called in batch mode. You must specify the loadfile."
  (let ((generated-autoload-file
	 (expand-file-name (car command-line-args-left))))
    (update-directory-autoloads default-directory)))

(defun would-like (pkg)
  "Helper for building rc files. Some modes do not have a provide."
  (condition-case nil (require pkg) (error nil)))
