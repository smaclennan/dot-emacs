;; -*- lexical-binding: t -*-

(defun sam-lisp-init (&optional compile)
  (interactive "P")
  (let* ((dir (file-name-directory (symbol-file 'sam-lisp-init 'defun)))
	 ;; must be called this for update-directory-autoloads
	 (generated-autoload-file (concat dir "sam-lisp-loaddefs.el"))
	 (sys (replace-regexp-in-string "gnu/" "" (symbol-name system-type)))
	 (sys-dir (expand-file-name (concat dir "../sys/")))
	 (sys-autoload (concat sys-dir "sam-sys-loaddefs.el")))

    ;; lisp dir
    (unless (file-exists-p generated-autoload-file)
      (message "Create %s..." generated-autoload-file)
      (loaddefs-generate dir generated-autoload-file))

    (add-to-list 'load-path dir t)
    (load generated-autoload-file t t)

    ;; sys dir
    (unless (file-exists-p sys-autoload)
      (let ((exclude (directory-files "." nil ".*\\.el")))
	(setq exclude (delete "sys-common.el" exclude))
	(setq exclude (delete (concat sys-type ".el") exclude))
	(loaddefs-generate default-directory (concat sys-dir sys ".el") exclude)))

    (add-to-list 'load-path sys-dir t)
    (load sys t)
    (load (format "compat-%d" emacs-major-version) t)

    (when compile
      (byte-recompile-directory dir 0))
    ))

