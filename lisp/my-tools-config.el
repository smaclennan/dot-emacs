;; Because this spans multiple packages... it makes sense to put it in
;; it's own file.

(require 'cc-mode)
(require 'my-tags)
(require 'git-diff)
(require 'kloc)
(require 'cl-extra)

(defun my-tools-fname (str fname)
  "Print a file, or directory, name."
  (when fname
    (princ (format "%-13s %s\n" str (abbreviate-file-name fname)))))

;;;###autoload
(defun my-tools-config (verbose)
  "Dump config information for the current buffer."
  (interactive "P")
  (with-output-to-temp-buffer "*my-config*"
    ;; Common
    (my-tools-fname "File name:" (buffer-file-name))
    (my-tools-fname "Git dir:" (git-dir nil t))
    (princ (format "Compile:      %s\n" compile-command))

    ;; C-ish files
    (when c-buffer-is-cc-mode
      (princ (format "C Style:      %s %s %d\n" c-indentation-style
		     (if indent-tabs-mode "tabs" "spaces") tab-width)))

    ;; optional files
    (unless (my-tools-fname "Tag file:" tags-file-name)
      (when (file-exists-p "TAGS")
	(my-tools-fname "Tag file:" "TAGS")))
    (my-tools-fname "My tag dir:" my-tags-dir)
    (my-tools-fname "My tag file:" my-tags-file)
    (my-tools-fname "Kloc:" (kloc-project-dir buffer-file-name))

    ;; verbose
    (when (and verbose (boundp 'my-compile-dir-list))
      (princ "\nmy-compile-dir-list:\n")
      (dolist (dir my-compile-dir-list)
	(princ (format "%S\n" dir))))))
