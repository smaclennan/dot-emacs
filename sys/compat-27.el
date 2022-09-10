(defalias 'pos-bol 'point-at-bol)
(defalias 'pos-eol 'point-at-eol)

(defun loaddefs-generate (dir output-file &optional excluded-files extra-data include-package-version generate-full)
  (if excluded-files
      ;; Hack for sys directory
      (let ((generated-autoload-file output-file))
	(update-file-autoloads (concat sys-type ".el") t)
	(update-file-autoloads "sys-common.el" t))
    (let ((generated-autoload-file output-file))
      (update-directory-autoloads dir))))
