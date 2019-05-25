;; This is meant to be used to compile lisp files in batch mode.

(dolist (dir '("lisp" "misc" "sys"))
  (add-to-list 'load-path (concat user-emacs-directory dir))
  (load (concat dir "-loaddefs") t t))

(load (format "compat-%d" emacs-major-version) t t)
(load (replace-regexp-in-string "gnu/" "" (symbol-name system-type)) t t)

(eval-when-compile (require 'autoload))

(defun update-loadfile ()
  "This is meant to be called in batch mode. You must specify the loadfile."
  (let ((generated-autoload-file
	 (expand-file-name (car command-line-args-left))))
    (update-directory-autoloads default-directory)))

(defun update-sys ()
  (let ((sys (replace-regexp-in-string "gnu/" "" (symbol-name system-type))))
    (update-file-autoloads (concat sys ".el") t
			   (expand-file-name "sys-loaddefs.el"))))

(defun would-like (pkg)
  "Helper for building rc files. Some modes do not have a provide."
  (condition-case nil (require pkg) (error nil)))
