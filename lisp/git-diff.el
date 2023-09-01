(provide 'git-diff)
(require 'my-grep)
(eval-when-compile (require 'ediff))

(defvar git-master "master"
  "With the change to main, allow overriding master")

;;;autoload
(defun git-cmd (cmd &rest args)
  "Apply the git command CMD with ARGS and output to the current buffer.
Error out if it fails."
  (unless (eq (apply 'call-process "git" nil t nil cmd args) 0)
    (error "git %s failed" cmd)))

(defun git-cat-doit (&optional rev)
  "Perform a git cat on the current buffer into a temporary buffer.
Returns the name of the buffer. If REV is not set, default is HEAD."
  ;; git-fname must be relative to git base
  (let* ((path (substring (buffer-file-name) (length (git-dir))))
	 (catname (concat rev ":" path))
	 (bufname (concat "*git-" (file-name-nondirectory path) "*")))
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer)
      (git-cmd "show" catname)
      (set-buffer-modified-p nil))
    bufname))

;;;###autoload
(defun git-cat (rev)
  "Perform a git cat on the current buffer into a temporary buffer.
If a prefix arg is specified, ask for the revision. Default is HEAD.
The name of the temporary buffer will be displayed."
  (interactive "P")
  (and rev (listp rev) ;; prefix arg
       (setq rev (read-from-minibuffer "Revision: ")))
  (message "git cat to buffer %s" (git-cat-doit rev)))

;;;###autoload
(defun git-ediff (rev)
  "Perform a git diff against the current buffer using ediff.
With a prefix arg, ask for the revision. With two prefix args,
use `git-master'. Otherwise defaults to HEAD."
  (interactive "P")
  (when rev
    (if (eq (car rev) 16)
	(setq rev git-master)
      (setq rev (read-from-minibuffer "Revision: "))))
  (ediff-buffers (current-buffer) (git-cat-doit rev)))

;;;###autoload
(defalias 'git-diff 'git-ediff)

;;;###autoload
(defun git-status ()
  (interactive)
  (with-current-buffer (get-buffer-create "*git status*")
    (erase-buffer)
    (git-cmd "status")))

;;;------ git grep

(defvar git-grep-top-of-tree t
  "*If non-nil, start grep at top of tree.")

;; Note: -w doesn't work as well as you would think :(
(defvar git-grep-args "--no-color -n"
  "*Default args to git grep.")

(defvar git-grep-filter nil
  "*Args to put after the search string.
Useful for specifying path filters. Must contain a leading space.

e.g. (setq git-grep-post-args \" -- :^Documentation\")
Would ignore the Documentation directory.")

(defvar git-grep-pipe-cmd nil
  "*Git grep output can be filtered through a pipe.")

(defvar git-grep-full-regexp t
  "*Does git grep support full regular expressions? Specifically \\b.")

;;;###autoload
(defun git-grep (arg str)
  "Run git grep, with user-specified regular expression, and collect
output in a buffer.  While grep runs asynchronously, you can use the
\\[next-error] command to find the text that grep hits refer to.

If `git-grep-top-of-tree' is non-nil, then the grep starts at the top
of the git dir. Else it starts at the current directory.

A prefix arg allows you to edit the grep command"
  (interactive "P\nsRegexp: ")
  (let ((cmd (concat "git --no-pager grep " git-grep-args " '" str "'" git-grep-filter)))
    (when git-grep-pipe-cmd
      (setq cmd (concat cmd " | " git-grep-pipe-cmd)))
    (when arg
      (setq cmd (read-string "Cmd: " cmd 'grep-history)))

    ;; GNU Emacs - must call this once or first grep fails
    (grep-compute-defaults)

    (if git-grep-top-of-tree
	(let ((default-directory (git-dir)))
	  (grep cmd))
      (grep cmd))))

;;;###autoload
(defun git-grep-at-point (arg)
  "Perform a `git-grep' with the word the point is on. If a region
exists, that is used rather than the current word. ARG has the same
meaning as in `git-grep'."
  (interactive "P")
  (let ((word (if mark-active
		  (buffer-substring (region-beginning) (region-end))
		(if git-grep-full-regexp
		    (concat "\\b" (my-grep-current-word) "\\b")
		  (my-grep-current-word)))))
    (git-grep arg word)))

;;;###autoload
(defun git-grep-toggle-top-of-tree ()
  "Toggle `git-grep-top-of-tree'."
  (interactive)
  (setq git-grep-top-of-tree (not git-grep-top-of-tree))
  (message "Git top of tree %s"
	   (if git-grep-top-of-tree "enabled" "disabled"))
  git-grep-top-of-tree)

(defun git-grep-tools-hook ()
  (when git-grep-filter
    (princ (format "%-22s %S\n" "git-grep-filter:" git-grep-filter)))
  (unless git-grep-top-of-tree
    (princ (format "%-22s disabled" "git-grep-top-of-tree"))))
(add-hook 'my-tools-hooks 'git-grep-tools-hook)
