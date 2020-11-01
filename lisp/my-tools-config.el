;; Because this spans multiple packages... it makes sense to put it in
;; it's own file.

(require 'cc-mode)

;;;###autoload
(defun my-tools-fname (str fname)
  "Print a file, or directory, name."
  (when fname
    (princ (format "%-22s %s\n" str (abbreviate-file-name fname)))))

(defvar my-tools-hooks nil
  "Hooks to call after `my-tools-config'.
Use princ + friends for output.")

;;;###autoload
(defun my-tools-config (verbose)
  "Dump config information for the current buffer."
  (interactive "P")
  (with-output-to-temp-buffer "*my-config*"
    ;; Common
    (my-tools-fname "File name:" (buffer-file-name))
    (my-tools-fname "Git dir:" (git-dir))
    (princ (format "%-22s %s\n" "Compile:" compile-command))

    ;; C-ish files
    (when c-buffer-is-cc-mode
      (princ (format "%-22s %s %s %d\n" "C Style:" c-indentation-style
		     (if indent-tabs-mode "tabs" "spaces") tab-width)))

    ;; tag files
    (when tags-table-list
      (princ (format "%-22s %S\n" "tags-table-list:" tags-table-list)))
    (when tags-file-name
      (princ (format "%-22s %s" "tags-file-name:" tags-file-name))
      (princ (if (local-variable-p 'tags-file-name) " (local)\n" "\n")))

    (run-hooks 'my-tools-hooks)

    ;; verbose
    (when (and verbose (boundp 'my-compile-dir-list))
      (princ "\nmy-compile-dir-list:\n")
      (dolist (dir my-compile-dir-list)
	(princ (format "%S\n" dir))))))

(provide 'my-tools-config)
