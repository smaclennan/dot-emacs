(defvar kloc-cmd "kwcheck run -F short -pd=%s %s"
  "*Klocwork command. Passed in project dir and file. Requires -F
short.")

(defvar kloc-dir nil
  "*Klocwork project directory. See also `kloc-dirs-list'.")

(defvar kloc-dirs-list nil
  "*A list of directories and their klocwork project. The
directory is a regular expression. The project can be an absolute
path or relative.

If `kloc-dir' is not set, `kloc-project-dir' will search this
list to try to find the kloc directory.")

(require 'compile)

;;;###autoload
(defun kloc-project-dir (file)
  "Try to find the project dir for FILE.
Checks `kloc-dir' and then `kloc-dirs-list'."
  (catch 'outer
    (let ((dir (file-name-directory (expand-file-name file)))
	  (kdir kloc-dir))
      ;; Try to lookup kdir
      (unless kdir
	(catch 'found
	  (dolist (one kloc-dirs-list)
	    (when (string-match (car one) dir)
	      (setq kdir (cadr one))
	      (throw 'found t)))
	    (throw 'outer nil)))
      ;; Absolute - make sure it exists
      (when (eq (string-to-char  kdir) ?/)
	(throw 'outer (if (file-exists-p kdir) kdir nil)))
      ;; Relative - walk up the directory tree
      (while (not (equal dir "/"))
	(when (file-exists-p (concat dir kdir))
	  (throw 'outer (concat dir kdir)))
	;; This removes the last directory
	(setq dir (file-name-directory (directory-file-name dir)))))))

(defun kloc-parse-buffer (start end)
  "Parse the current buffer from START to END for klocwork
output. Returns point after header."
  ;; Skip the header
  (let (saved)
    (goto-char start)
    (when (or (re-search-forward "Linking stage completed" end t)
	      (re-search-forward "up to date" end t))
      (end-of-line) (forward-char))
    (setq saved (point)) ;; always set it to something

    (while (re-search-forward "^[^:]+:[0-9]+" nil t)
      (goto-char (match-end 0))
      (insert ":1"))

    saved))

;;;###autoload
(defun kloc ()
  "Check the current buffer with klocwork.
Uses `kloc-project-dir' to find the project directory. Puts the
results in a compilation buffer."
  (interactive)
  (let* ((kdir (kloc-project-dir buffer-file-name))
	 (cmd (format kloc-cmd kdir buffer-file-name))
	 start)
    (unless kdir (error "No project directory found"))
    (with-current-buffer (get-buffer-create "*kloc*")
      (erase-buffer)
      (message "%s" cmd)
      (shell-command cmd t t)
      (setq start (kloc-parse-buffer (point-min) (point-max)))
      (compilation-mode "kloc")
      (setq buffer-read-only nil)
      (compilation--parse-region start (point-max)))
    (display-buffer "*kloc*"))
  (message "kloc done."))

(provide 'kloc)
