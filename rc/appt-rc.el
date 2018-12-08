;; Use (diary) to display it
(setq appt-display-diary nil)

;; We need the full path for matching
(setq diary-file (expand-file-name diary-file))

;; Update appt if the diary file changed
(defun check-if-diary-changed ()
  (when (equal (buffer-file-name) diary-file)
    (appt-check t)))
(add-hook 'after-revert-hook 'check-if-diary-changed)

;; Enable auto-revert-mode for the diary file
(with-current-buffer (find-file-noselect diary-file)
  (auto-revert-mode 1))
