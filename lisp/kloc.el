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

;; Defined in rc/compile.el
(defvar make-clean-command nil)

(require 'compile)
(require 'git-diff)

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

(defun kloc-do-one (file &optional no-parse-compile)
  "Run kloc on FILE into the current buffer at the point.
If NO-PARSE-COMPILE is nil, also do a
`compilation--parse-region' on the entire buffer.

Returns the kloc project directory or nil."
  (let ((kdir (kloc-project-dir file))
	(start (point)))
    (when kdir
      (save-excursion
	(call-process-shell-command (format kloc-cmd kdir file) nil '(t t) t))

      ;; Delete the header
      (when (or (re-search-forward "Linking stage completed" nil t)
		(re-search-forward "up to date" nil t))
	(end-of-line) (forward-char))
      (kill-region start (point))

      ;; Fixup the lines for compilation
      (while (re-search-forward "^[^:]+:\\([0-9]+\\)" nil t)
	(replace-match (concat file ":" (match-string 1) ":1"))))
    ;; For the kloc-do-many list we want to parse even if no kdir.
    ;; Doesn't hurt in any case.
    (unless no-parse-compile
      (compilation-mode "kloc")
      (setq buffer-read-only nil)
      (compilation--parse-region (point-min) (point-max)))
    kdir))

(defun kloc-do-list (flist)
  "Perform kloc on a list of files and put the results in one buffer."
    (with-current-buffer (get-buffer-create "*kloc*")
      (erase-buffer)
      (display-buffer "*kloc*")
      (while flist
	(goto-char (point-max))
	(insert "Analyzing " (car flist) "...\n")
	(unless (kloc-do-one (car flist) (cdr flist))
	  (insert "%s has no project directory" (car flist)))
	(setq flist (cdr flist)))))

;;;###autoload
(defun kloc ()
  "Check the current buffer with klocwork.
Uses `kloc-project-dir' to find the project directory. Puts the
results in a compilation buffer."
  (interactive)
  (let ((file buffer-file-name))
    (with-current-buffer (get-buffer-create "*kloc*")
      (erase-buffer)
      (unless (kloc-do-one file)
	(error "No project directory found")))
    (display-buffer "*kloc*")
    (message "kloc done.")))

(defun kloc-run (cmd)
  "Trivial helper function."
  (message "%s" cmd)
  (shell-command cmd))

;;;###autoload
(defun kloc-add-local ()
  "Add the current buffers file to a kloc project locally.

Assumes that `compile-command' and `make-clean-command' are set
properly for the current buffer.

WARNING: This tends to mess up the project."
  (interactive)
  (let ((kdir (kloc-project-dir buffer-file-name)))
    (unless kdir (error "No klocwork project found"))
    (kloc-run make-clean-command)
    (kloc-run (concat "kwinject -o buildspec.out " compile-command))
    (kloc-run (concat"kwcheck run -b buildspec.out -pd=" kdir))
    (message "done")))

;;;###autoload
(defun kloc-git ()
  "Diff the branch against master and run klocwork on all the C
files... collecting the results in one buffer."
  (interactive)
  (with-temp-buffer
    ;; We must be rooted in the git dir
    (let ((default-directory (git-dir)) flist)
      ;; Collect the file names
      (erase-buffer)
      (shell-command "git diff master --stat" t)
      (goto-char (point-min))
      (while (re-search-forward "^ \\([^0-9][^ ]*\\.c\\)" nil t)
	(setq flist (append flist (list (expand-file-name (match-string 1))))))
      (if flist
	  (progn
	    (kloc-do-list flist)
	    (message "Kloc done"))
	(message "No diffs found")))))

(provide 'kloc)
