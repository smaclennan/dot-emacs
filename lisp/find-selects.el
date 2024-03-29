;; -*- lexical-binding: t -*-
(require 'my-compile)

(defvar kfs-bufname "*find-selects*"
  "*Name of buffer to use for `kernel-find-selects' and friends.")

(defvar kfs-kernel-dir my-kernel-dir
  "Kernel directory to use for `kernel-find-selects'.")

(defun kfs-find-in-file(select file)
  "Returns t if `SELECT' is found as a select in `FILE' (which should
be a Kconfig)."
  (let (match type found)
    (set-buffer (get-buffer-create kfs-bufname))
    (erase-buffer)
    (insert-file-contents file)
    (goto-char (point-min))
    (while (re-search-forward "^\\(\tselect\\|config\\|menuconfig\\)\\(.*$\\)" nil t)
      (setq type (match-string 1))
      (setq match (match-string 2))
      (when (string-match select match)
	(if (string= type "\tselect")
	    (unless found (setq found "  "))
	  (setq found "+ "))))
    found))

(defun kfs-kconfigs(kdir select)
  "Find all the Kconfig files in `KDIR' and check them against
`SELECT'. Returns the list of matching Kconfigs."
  (let ((kconfig (concat kdir "/Kconfig"))
	kfs-list type)
    (when (file-exists-p kconfig)
      (setq type (kfs-find-in-file select kconfig))
      (when type
	(setq kfs-list (list (concat type kconfig)))))
    (dolist (dir (directory-files kdir t "^[a-zA-Z0-9]" t))
      (when (file-directory-p dir)
	(setq kfs-list (nconc kfs-list (kfs-kconfigs dir select)))))
    kfs-list))

;;;###autoload
(defun kernel-find-selects(select &optional kdir)
  "Search all the Kconfig files in `KDIR' for select and config lines
that match `SELECT'.

If `KDIR' is nil, `kfs-kernel-dir' is used. Outputs the file list
to the *find-selects* buffer."
  (interactive "sConfig: ")
  (unless kdir
    (setq kdir kfs-kernel-dir))

  (setq select (concat "\\<" select "\\>"))

  (let ((case-fold-search nil)
	kfs-list)
    (save-excursion
      (message "Searching...")
      (setq kfs-list (kfs-kconfigs kdir select))
      (message ""))
    (if kfs-list
	(let ((buf (switch-to-buffer kfs-bufname)))
	  (erase-buffer)
	  (dolist (file kfs-list)
	    (princ (concat file "\n") buf)))
      (when (bufferp (get-buffer kfs-bufname))
	(kill-buffer kfs-bufname))
      (message "Nothing found."))))
