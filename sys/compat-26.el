;; Used in Slackware 15
;; WARNING: compat-26,27,28 are linked together!!!
;; WARNING: compat-24,25 include this file

(load "compat-30")

;; All these changes where introduced in 29

(defalias 'pos-bol 'point-at-bol)
(defalias 'pos-eol 'point-at-eol)

(defalias 'elide-head-mode 'elide-head)

(defun loaddefs-generate (dir output-file &optional excluded-files extra-data include-package-version generate-full)
  (if excluded-files
      ;; Hack for sys directory
      (let ((generated-autoload-file output-file))
	(update-file-autoloads (concat sys-type ".el") t)
	(update-file-autoloads "sys-common.el" t))
    (let ((generated-autoload-file output-file))
      (update-directory-autoloads dir))))
