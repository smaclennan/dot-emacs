(require 'smerge)

;; Tested with git version 2.17.1

(defvar smerge-git-buffer "*smerge git diff*"
  "The buffer to use for `smerge-git' or nil for unique buffers.

Since `smerge-git' can end up creating a lot of diff buffers, it
is generally easier to put all of them in one buffer. But setting
this to nil will create unique buffers ala `git-ediff'.")

(defvar smerge-git-dir)
(defvar smerge-git-branch)

(define-minor-mode smerge-git-mode
  "Minor mode for smerge git buffer."
  nil " git-smerge"
  '(([mouse-1]	. smerge-git-diff)
    ([mouse-2]	. smerge-git-diff)
    ("\C-m"	. smerge-git-diff)
    ("g"	. smerge-git-reload)
    ("r"	. smerge-git-reload)
    ("n"	. smerge-next)
    ("p"	. smerge-prev)))

;;;###autoload
(defun smerge-git (branch)
  "Git diff the current branch against another branch."
  (interactive "sBranch: ")
  (unless (setq smerge-git-dir (git-dir)) (error "No .git directory found"))
  (setq smerge-git-branch branch)
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

    (switch-to-buffer smerge-buffer) ;; output buffer
    (setq buffer-read-only nil) ;; writable
    (erase-buffer)
    (setq smerge-git-mode t)

    (let (start extent)
      ;; Only in 1 (new)
      (goto-char (point-min))
      (dolist (path new)
	(setq start (point))
	(insert path)
	(setq extent (smerge-git-make-extent start (point) 'smerge-only1-face))
	(overlay-put extent 'type 1)
	(insert "\n")
	)

      ;; Both
      (dolist (path both)
	(move-to-column 20 t)
	(setq start (point))
	(insert path)
	(setq extent (smerge-git-make-extent start (point) 'smerge-diff-face))
	(overlay-put extent 'type 3)
	(insert "\n")
	)


      ;; Only in 2
      (dolist (path old)
	(move-to-column 40 t)
	(setq start (point))
	(insert path)
	(setq extent (smerge-git-make-extent start (point) 'smerge-only2-face))
	(overlay-put extent 'type 2)
	(insert "\n")
	))
    (setq buffer-read-only t) ;; read-only
    (goto-char (point-min))))

(defun smerge-git-show (path)
  (let ((bufname (if smerge-git-buffer
		     smerge-git-buffer
  		   (concat "*git-" (file-name-nondirectory path) "*"))))
    (with-current-buffer (get-buffer-create bufname)
      (erase-buffer)
      (call-process "git" nil t nil "show" (concat smerge-git-branch ":" path)))
    bufname))

(defun smerge-git-diff ()
  (interactive)
  (let ((extent (smerge-nearest-extent (point))))
    (unless extent (error "No extent at point"))
    (let ((type (overlay-get extent 'type))
	  (file (smerge-file extent))
	  (default-directory smerge-git-dir))
      (cond
       ((eq type 3)
	(ediff-buffers (find-file-noselect file) (smerge-git-show file)))
       ((eq type 1)
	(find-file file)
	(goto-char (point-min))
	(delete-other-windows)
	(message "New file"))
       ((eq type 2)
	(message "File deleted"))))))

(defun smerge-git-reload ()
  (interactive)
  (smerge-git smerge-git-branch))

(defun smerge-git-make-extent (start end face)
  (smerge-make-extent start end face smerge-git-mode-map))
