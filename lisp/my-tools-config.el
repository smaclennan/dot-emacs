;; -*- lexical-binding: t -*-
(require 'cc-mode)

(require 'git-diff)

(defvar my-tools-hooks nil
  "Hooks to call during `my-tools-config'.
Use princ + friends for output.")

;;;###autoload
(defun my-tools-fname (str fname)
  "Print a file, or directory, name."
  (when fname
    (princ (format "%-22s %s\n" str (abbreviate-file-name fname)))))

;;;###autoload
(defun my-tools-str (str val)
  (when val
    (princ (format "%-22s %S\n" str val))))

;;;###autoload
(defun my-tools-config (verbose)
  "Dump config information for the current buffer."
  (interactive "P")
  (with-output-to-temp-buffer "*my-config*"
    ;; Common
    (my-tools-fname "File name:" (buffer-file-name))
    (my-tools-fname "Git dir:" (git-dir))
    (my-tools-str "git-grep-pipe-cmd:" git-grep-pipe-cmd)
    (my-tools-str "Compile:" compile-command)

    ;; C-ish files
    (when c-buffer-is-cc-mode
      (princ (format "%-22s %s %s %d\n" "C Style:" c-indentation-style
		     (if indent-tabs-mode "tabs" "spaces") tab-width)))

    ;; tag files
    (my-tools-str "tags-table-list:" tags-table-list)
    (my-tools-str "tags-file-name:" tags-file-name)

    (run-hooks 'my-tools-hooks)

    ;; verbose
    (when (and verbose (boundp 'my-compile-dir-list))
      (princ "\nmy-compile-dir-list:\n")
      (dolist (dir my-compile-dir-list)
	(princ (format "%S\n" dir))))))

(provide 'my-tools-config)
