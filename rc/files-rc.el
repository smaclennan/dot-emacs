;; find-file is files

;; When was the last time you wanted to edit a .elc file?
(defun my-find-file-predicate (file)
  (not (string= (file-name-extension file) "elc")))

(defun find-file-read-args (prompt mustmatch)
  (list (read-file-name prompt nil default-directory mustmatch nil
			'my-find-file-predicate)
	t))

(defvar commit-names '("COMMIT_EDITMSG" "svn-commit.tmp" "README")
  "* List of commit buffer names.")

(defun check-for-commit ()
  "If this is a commit buffer, set to text mode."
  (when (eq major-mode 'fundamental-mode)
    (let ((buff (buffer-name)))
      (dolist (name commit-names)
	(when (string= buff name)
	  (text-mode))))))
(add-hook 'find-file-hook 'check-for-commit t)

(if (package-installed-p 'markdown-mode)
    (add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode) t)
  (add-to-list 'auto-mode-alist '("\\.md\\'" . text-mode) t))

;; There are going to be large tag files
(setq large-file-warning-threshold #x5600000) ;; 86M
