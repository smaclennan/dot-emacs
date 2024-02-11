;; SAM -*- lexical-binding: nil -*- until I work out the warning

;; find /usr/include -name "*.h" | etags --no-members -o ~/.INCLUDE-TAGS -

;; 30M on my machine --no-members saves about 9M
(defvar tag-includes-file (expand-file-name "~/.INCLUDE-TAGS")
  "Where to put the tag file for /usr/include.")

;;;###autoload
(defun tag-includes-update ()
  "Update the `tag-includes-file' for /usr/include.
This may take a while, but should not need to be done often."
  (interactive)
  (message "%s %s. Please wait..."
	   (if (file-exists-p tag-includes-file) "Updating" "Creating")
	   tag-includes-file)
  (shell-command
   (format "find /usr/include -type f | etags --no-members -o %s -"
	   tag-includes-file))
  (message "Done tag update"))

;;;###autoload
(defun tag-includes (define)
  "Lookup DEFINE in `tag-includes-file'.
Interactively, defaults to the word at the point. Will create the
tag file if it doesn't exist."
  (interactive
   (list (let ((word (current-word)))
	   (read-string (concat "Define [" word "]: ") nil nil word))))
  (unless (file-exists-p tag-includes-file) (tag-includes-update))
  (let ((tags-file-name nil)
	(tags-table-list nil)
	(tags-completion-table nil))
    (visit-tags-table-buffer tag-includes-file)
    (xref-find-definitions define)))

(provide 'tag-includes)
