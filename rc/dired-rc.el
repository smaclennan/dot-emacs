(define-key dired-mode-map "\C-cu" 'dired-do-du)
(define-key dired-mode-map "\C-ca" 'dired-toggle-all)

(setq dired-listing-switches "-l")
(setq dired-no-confirm '(kill-file-buffer))

;;;###autoload
(defun dired-do-du ()
  "Call `du -s' on the current directory or file."
  (interactive)
  (let* ((dir (dired-get-filename))
	(cmd (concat "du -sh " dir)))
    (message "%s" (shell-command-to-string cmd))))

;;;###autoload
(defun dired-toggle-all (force)
  "Toggle showing dot-files."
  (interactive "P")
  (if (or force (string-match "a" dired-listing-switches))
      (while (string-match "a" dired-listing-switches)
	(setq dired-listing-switches
	      (replace-match "" nil nil dired-listing-switches)))
    (setq dired-listing-switches (concat dired-listing-switches "a")))
  (dired-sort-other dired-listing-switches))
