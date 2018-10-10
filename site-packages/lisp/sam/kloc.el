(defvar kloc-dir nil "*Klocwork project directory.")
(defvar kloc-cmd "kwcheck run -F short"
  "*Klocwork command. Requires -F short.")

(require 'compile)

;;;###autoload
(defun kloc ()
  "Check the current buffer with klocwork."
  (interactive)
  (let ((buff (get-buffer-create "*kloc*"))
	(cmd (concat kloc-cmd " -pd=" kloc-dir " " buffer-file-name))
	start)
    (save-current-buffer (set-buffer buff) (erase-buffer))
    (message "%s" cmd)
    (shell-command cmd buff buff)
    (with-current-buffer buff

      ;; Skip the header
      (goto-char (point-min))
      (when (or (re-search-forward "Linking stage completed" nil t)
		(re-search-forward "up to date" nil t))
	(end-of-line) (forward-char))
      (setq start (point)) ;; always set it to something

      (while (re-search-forward "^[^:]+:[0-9]+" nil t)
	(goto-char (match-end 0))
	(insert ":1"))

      (compilation-mode "kloc")
      (setq buffer-read-only nil)
      (compilation--parse-region start (point-max)))
    (display-buffer buff))
  (message "kloc done."))

;;;###autoload
(defun kloc-set-dir (dir)
  "Set the `kloc-dir' variable, prompts with current setting."
  (interactive
   (list (read-directory-name "Dir: " kloc-dir kloc-dir t)))
  (setq kloc-dir (expand-file-name dir)))

(provide 'kloc)
