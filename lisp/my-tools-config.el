;; Because this spans multiple packages... it makes sense to put it in
;; it's own file.

(require 'cc-mode)
(require 'my-tags)
(require 'git-diff) ;; for git-dir
(require 'kloc)

;;;###autoload
(defun my-tools-config (verbose)
  "Dump config information for the current buffer."
  (interactive "P")
  (with-output-to-temp-buffer "*my-config*"
    ;; Common
    (princ (format "File name:    %S\n" (buffer-file-name)))
    (let ((git-dir (condition-case nil (git-dir) (error nil))))
      (princ (format "Git dir:      %S\n" git-dir)))
    (princ (format "Compile:      %S\n" compile-command))

    ;; C-ish files
    (when (and (boundp 'c-buffer-is-cc-mode) c-buffer-is-cc-mode)
      (princ (format "C Style:      %S" c-indentation-style))
      (princ (if indent-tabs-mode " tabs " " spaces "))
      (if (eq c-basic-offset tab-width) ;; 99.99% case
	  (princ (format "%d\n" c-basic-offset))
	(princ (format "%d/%d\n" c-basic-offset tab-width))))

    ;; optional files
    (if tags-file-name
	(princ (format "Tag file:     %S %s\n"
		       tags-file-name
		       (if (file-exists-p tags-file-name) "OK" "missing")))
      (if (file-exists-p "TAGS")
	  (princ (format "Tag file:     \"TAGS\"\n"))))
    (condition-case nil
	(when (or my-tags-dir my-tags-file)
	  (princ (format "My tag dir:   %S\n" my-tags-dir))
	  (princ (format "My tag file:  %S\n" my-tags-file)))
      (error nil))
    (when (local-variable-p 'kloc-dir)
      (princ (format "Kloc:         %S\n" kloc-dir)))

    ;; verbose and C
    (when (and verbose (boundp 'my-compile-dir-list))
      (require 'cl-extra)
      (princ "\nmy-compile-dir-list:")
      (princ (with-temp-buffer
	       (cl-prettyprint my-compile-dir-list)
	       (buffer-string))))
    ))

