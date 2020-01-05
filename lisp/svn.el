(provide 'svn)

(require 'compile)

(defun svn-cat-doit (&optional rev)
  "Perform an svn cat -r rev on the current buffer into a temporary buffer.
If a prefix arg is specified, ask for the revision.
When called interactively, the name of the temporary buffer will be displayed."
  (let* ((fname (file-name-nondirectory (buffer-file-name)))
	 (buf (get-buffer-create (concat "*" fname "*")))
	 (catname fname))

    (if rev (setq catname (concat catname "@" rev)))

    (save-current-buffer
      (set-buffer buf)
      (erase-buffer)
      (call-process "svn" nil buf nil "cat" catname)
      (set-buffer-modified-p nil))
    buf))

;;;###autoload
(defun svn-cat (rev)
  "Perform an svn cat -r rev on the current buffer into a temporary buffer.
If a prefix arg is specified, ask for the revision.
The name of the temporary buffer will be displayed."
  (interactive "P")
  (and rev (listp rev) ;; prefix arg
       (setq rev (read-from-minibuffer "Revision: ")))
  (message "svn cat to buffer %s" (buffer-name (svn-cat-doit rev))))

;;;###autoload
(defun svn-diff (rev)
  "Perform an svn diff against the current buffer using ediff.
With a prefix arg, ask for the revision(s)."
  (interactive "P")
  (if (and rev (listp rev))
      (progn
	(setq rev (read-from-minibuffer "Revision: "))
	(if (string-match "\\([0-9]+\\):\\([0-9]+\\)" rev)
	    (ediff-buffers (svn-cat-doit (match-string 2 rev))
			   (svn-cat-doit (match-string 1 rev)))
	  (ediff-buffers (current-buffer) (svn-cat-doit rev))))
    (ediff-buffers (current-buffer) (svn-cat-doit))))


;;;###autoload
(defalias 'svn-ediff 'svn-diff)

(defun svn-cmd (cmd)
  (let* ((fname (file-name-nondirectory (buffer-file-name)))
	 (buff (get-buffer-create "*svn output*"))
	 (outwin (display-buffer buff)))
    (compilation-set-window-height outwin)
    (call-process "svn" nil buff t cmd fname)
    (revert-buffer nil t t)))


;;;###autoload
(defun svn-revert ()
  "Perform an svn revert on the current buffer."
  (interactive)
  (svn-cmd "revert"))

;;;###autoload
(defun svn-up ()
  "Perform an svn up on the current buffer."
  (interactive)
  (svn-cmd "up"))

;;;###autoload
(defun svn-url (&optional show)
  "Perform an svn info on the current buffer to get the svn url.
Output will be nil if this directory not in svn."
  (interactive "p")
  ;; Note: we must save dir before the set-buffer
  (let (url (dir default-directory))
    (save-current-buffer
      (set-buffer (get-buffer-create "*svn output*"))
      (erase-buffer)
      (call-process "svn" nil t nil "info" dir)
      (goto-char (point-min))
      (if (re-search-forward "^URL: \\(.+\\)" nil t)
	  (setq url (concat (match-string 1) "/")))
      (if show (message "URL: %S" url))
      url)))

;;;###autoload
(defun svn-stat ()
  "Perform an `svn stat' in the directory of the current buffer."
  (interactive)
  (let ((buff (get-buffer-create "*svn output*")))
    (save-current-buffer
      (switch-to-buffer buff)
      (erase-buffer))
    (message "svn stat....")
    (call-process "svn" nil buff t "stat")
    (if (eq 0 (buffer-size buff))
	(message "Up to date.")
      (message "Done.")
      (compilation-set-window-height (display-buffer buff)))
    ))

;;;###autoload
(defun svn-revision ()
  (interactive)
  (let (rev)
    (with-temp-buffer
      (when (eq (call-process "svn" nil t nil "info") 0)
	(re-search-backward "^Revision: \\([0-9]+\\)$")
	(setq rev (string-to-number (match-string 1)))
	(when (called-interactively-p 'interactive)
	  (message "rev %d" rev))))
    rev))

;;;###autoload
(defun svn-git-massage (svn-revision)
  "Try to convert a git diff to an svn diff.

This is geared towards reviewboard, so it needs an SVN-REVISION.

Currently assumes the two paths are the same.
Untested with additions or deletions."
  (interactive
   (list (number-to-string (read-number "SVN revision: " (svn-revision)))))
  (goto-char (point-min))
  (while (re-search-forward "^diff --git a/\\([^ ]+\\)" nil t)
    (let ((start (match-beginning 0))
	  (path (match-string 1)))
      (forward-line 3)
      (unless (looking-at (concat "^+++ b/" path)) (error "PROBS"))
      (forward-line 1)
      (kill-region start (point))
      (insert "Index: " path "\n"
	      (make-string 67 ?=) "\n"
	      "--- " path "\t(revision " svn-revision ")\n"
	      "+++ " path "\t(working copy)\n"))))
