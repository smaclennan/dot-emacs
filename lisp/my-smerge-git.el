(require 'my-smerge)

;; Tested with git version 2.17.1

(defvar my-smerge-git-buffer "*my-smerge git diff*"
  "The buffer to use for `my-smerge-git' or nil for unique buffers.

Since `my-smerge-git' can end up creating a lot of diff buffers, it
is generally easier to put all of them in one buffer. But setting
this to nil will create unique buffers ala `git-ediff'.")

(defvar my-smerge-git-dir)
(defvar my-smerge-git-branch)

(define-minor-mode my-smerge-git-mode
  "Minor mode for my-smerge git buffer."
  :lighter " git-my-smerge"
  :keymap '(([mouse-1]	. my-smerge-git-diff)
	    ("\C-m"	. my-smerge-git-diff)
	    ("n"	. my-smerge-git-next)
	    ([f4]	. my-smerge-git-next)))

;;;###autoload
(defun my-smerge-git (branch)
  "Git diff the current branch against another branch."
  (interactive "sBranch: ")
  (unless (setq my-smerge-git-dir (git-dir)) (error "No .git directory found"))
  (setq my-smerge-git-branch branch)
  (let (new both old path)
    (with-temp-buffer
      (setq default-directory (git-dir)) ;; start at root
      (call-process "git" nil t nil "diff" branch)
      (goto-char (point-min))
      (while (re-search-forward "^diff --git a/\\([^ ]+\\)" nil t)
	(setq path (match-string 1))
	(forward-line)
	(cond
	 ((looking-at "new file") (setq new (nconc new (list path))))
	 ((looking-at "deleted file") (setq old (nconc old (list path))))
	 (t (setq both (nconc both (list path)))))))

    (switch-to-buffer my-smerge-buffer) ;; output buffer
    (setq buffer-read-only nil) ;; writable
    (erase-buffer)
    (setq my-smerge-git-mode t)

    (let (start extent)
      ;; Only in 1 (new)
      (goto-char (point-min))
      (dolist (path new)
	(setq start (point))
	(insert path)
	(setq extent (my-smerge-git-make-extent start (point) 'my-smerge-only1-face))
	(overlay-put extent 'type 1)
	(insert "\n")
	)

      ;; Both
      (dolist (path both)
	(move-to-column 20 t)
	(setq start (point))
	(insert path)
	(setq extent (my-smerge-git-make-extent start (point) 'my-smerge-diff-face))
	(overlay-put extent 'type 3)
	(insert "\n")
	)


      ;; Only in 2
      (dolist (path old)
	(move-to-column 40 t)
	(setq start (point))
	(insert path)
	(setq extent (my-smerge-git-make-extent start (point) 'my-smerge-only2-face))
	(overlay-put extent 'type 2)
	(insert "\n")
	))
    (setq buffer-read-only t) ;; read-only
    (goto-char (point-min))))

(defun my-smerge-git-show (path)
  (let ((bufname (if my-smerge-git-buffer
		     my-smerge-git-buffer
  		   (concat "*git-" (file-name-nondirectory path) "*"))))
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer)
      (call-process "git" nil t nil "show" (concat my-smerge-git-branch ":" path)))
    bufname))

(defun my-smerge-git-diff ()
  (interactive)
  (let ((extent (my-smerge-nearest-extent (point))))
    (unless extent (error "No extent at point"))
    (let ((type (overlay-get extent 'type))
	  (file (my-smerge-file extent))
	  (default-directory my-smerge-git-dir))
      (cond
       ((eq type 3)
	(ediff-buffers (find-file-noselect file) (my-smerge-git-show file)))
       ((eq type 1)
	(find-file file)
	(goto-char (point-min))
	(delete-other-windows)
	(message "New file"))
       ((eq type 2)
	(message "File deleted"))))))

(defun my-smerge-git-next ()
  "Move to next file or diff."
  (interactive)
  (forward-line 1)
  (my-smerge-git-diff))

(defun my-smerge-git-make-extent (start end face)
  (my-smerge-make-extent start end face my-smerge-git-mode-map))
