(provide 'git-diff)

(eval-when-compile
  (require 'cl)
  (require 'ediff)
  (unless (fboundp 'region-exists-p)
    (defsubst region-exists-p () mark-active)))

(defun git-dir (&optional dir)
  "Find the base git directory. If DIR is nil, `default-directory' is used."
  (unless dir (setq dir default-directory))
  ;; Sanitize the directory
  (setq dir (expand-file-name (file-name-as-directory dir)))
  (catch 'found
    (while (not (equal dir "/"))
      (when (file-exists-p (concat dir ".git"))
	(throw 'found dir))
      ;; This removes the last directory
      (setq dir (file-name-directory (directory-file-name dir))))
    (error "No git base.")))

(defun git-cat-doit (&optional rev buf)
  "Perform a git cat on the current buffer into a temporary buffer.
If `rev' is not set, default is HEAD."
  (let ((full-path (buffer-file-name)) catname)
    (unless full-path (error "Buffer %s does not have a path" (buffer-name)))
    ;; git-fname must be relative to git base
    (setq catname (concat rev ":" (substring full-path (length (git-dir)))))
    (unless buf
      (let ((bufname (concat "*" (file-name-nondirectory full-path) "*")))
	(setq buf (get-buffer-create bufname))))
    (with-current-buffer buf
      (erase-buffer)
      (call-process "git" nil buf nil "show" catname)
      (set-buffer-modified-p nil))
    buf))

;;;###autoload
(defun git-cat (rev)
  "Perform a git cat on the current buffer into a temporary buffer.
If a prefix arg is specified, ask for the revision. Default is HEAD.
The name of the temporary buffer will be displayed."
  (interactive "P")
  (and rev (listp rev) ;; prefix arg
       (setq rev (read-from-minibuffer "Revision: ")))
  (message "git cat to buffer %s" (buffer-name (git-cat-doit rev))))

;;;###autoload
(defun git-ediff (rev)
  "Perform a git diff against the current buffer using ediff.
With a prefix arg, ask for the revision. With two prefix args,
use 'master'. Otherwise defaults to HEAD."
  (interactive "P")
  (when rev
    (if (eq (car rev) 16)
	(setq rev "master")
      (setq rev (read-from-minibuffer "Revision: "))))
  (ediff-buffers (current-buffer) (git-cat-doit rev)))

;;;###autoload
(defalias 'git-diff 'git-ediff)

;;;###autoload
(defun git-status ()
  (interactive)
  (let ((buf (get-buffer-create "*git status*")))
    (call-process "git" nil buf nil "status")))

;;;------ git grep

(defvar git-grep-top-of-tree t
  "*If non-nil, start grep at top of tree.")

;; Note: -w doesn't work as well as you would think :(
(defvar git-grep-args "--no-color -n"
  "*Default args to git grep.")

(defvar git-grep-pipe-cmd "cat"
  "*Git grep output is usually piped through cat, but other
commands can be specified to, for example, filter the git grep
output.")

;;;###autoload
(defun git-grep (arg str)
  "Run git grep, with user-specified regular expression, and collect
output in a buffer.  While grep runs asynchronously, you can use the
\\[next-error] command to find the text that grep hits refer to.

If `git-grep-top-of-tree' is non-nil, then the grep starts at the top
of the git dir. Else it starts at the current directory.

A prefix arg allows you to edit the grep command"
  (interactive "P\nsRegexp: ")
  (let ((cmd (concat "git grep " git-grep-args " '" str "' | " git-grep-pipe-cmd))
	(grep-null-device nil) ;; XEmacs
	(grep-use-null-device nil)) ;; Emacs
    (when arg
      (setq cmd (read-string "Cmd: " cmd 'grep-history)))

    ;; GNU Emacs - must call this once or first grep fails
    (grep-compute-defaults)

    (if git-grep-top-of-tree
	(let ((default-directory (git-dir))
	      (grep-use-null-device nil)) ;; Emacs
	  (grep cmd))
      (grep cmd))))

;;;###autoload
(defun git-grep-at-point (arg)
  "Perform a `git-grep' with the word the point is on. If a region
exists, that is used rather than the current word. ARG has the same
meaning as in `git-grep'."
  (interactive "P")
  (let ((word (if (region-exists-p)
		  (buffer-substring (region-beginning) (region-end))
		(current-word))))
    (git-grep arg word)))

;;;###autoload
(defun git-grep-toggle-top-of-tree ()
  "Toggle `git-grep-top-of-tree'."
  (interactive)
  (setq git-grep-top-of-tree (not git-grep-top-of-tree))
  (message "Git top of tree %s"
	   (if git-grep-top-of-tree "enabled" "disabled"))
  git-grep-top-of-tree)
