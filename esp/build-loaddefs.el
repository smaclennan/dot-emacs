(eval-when-compile (require 'cl))

(defun build-loaddefs (&optional dir loaddefs)
  (unless dir
    (setq dir (pwd))
    (string-match "^Directory \\(.*\\)/" dir)
    (setq dir (match-string 1 dir)))
  (unless loaddefs
    (setq loaddefs (concat dir "/" (file-name-nondirectory dir) "-loaddefs.el")))
  (loop for file in (directory-files dir t "^[^._].*\.el$") do
	(update-file-autoloads file t loaddefs)))
