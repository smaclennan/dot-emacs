;; Handy Dandy(tm) Functions

;;;###autoload
(defun unixtime (seconds)
  "Convert SECONDS in Unix time to human readable date."
  (interactive "nTime: ")
  (message (format-time-string "%a %b %d %T %Z %Y" (seconds-to-time seconds))))

;;;###autoload
(defun my-x-colour (number)
  (interactive "sColour: ")
  (let ((case-fold-search t))
    (cond
     ;; Convert 'd d d' to `#xxxxxx'
     ((string-match "^\\([0-9]+\\)\\([ \t]+[0-9]+\\)\\([ \t]+[0-9]+\\)$" number)
      (message "#%02x%02x%02x"
	       (string-to-number (match-string 1 number))
	       (string-to-number (match-string 2 number))
	       (string-to-number (match-string 3 number))))
     ;; Convert `#xxxxxx' to `d d d'
     ((string-match
       "^#\\([0-9a-f]\\{2\\}\\)\\([0-9a-f]\\{2\\}\\)\\([0-9a-f]\\{2\\}\\)$"
       number)
      (message "%d %d %d"
	       (string-to-number (match-string 1 number) 16)
	       (string-to-number (match-string 2 number) 16)
	       (string-to-number (match-string 3 number) 16)))
     (t (error "Invalid")))))

;;;###autoload
(defun size-window (size)
  (let* ((edges (window-edges))
	 (cursize (- (nth 3 edges) (nth 1 edges) -1)))
    (unless (= size cursize)
      (enlarge-window (- size cursize)))))

;; For when you need a good excuse...
;; From BOFH
(defvar excuse-phrase-file (concat user-emacs-directory "lisp/excuses.lines")
  "*File containing excuses")

;;;###autoload
(defun excuse (&optional insert)
  "Return or display a random excuse.  With prefix arg, insert it."
  (interactive "P")
  (let ((case-fold-search nil)
	(excuse (concat (cookie excuse-phrase-file "I didn't" "do it"))))
    (if (string-match "^[^A-Z]" excuse)
	(setq excuse (concat "The problem is " excuse)))
    (if (string-match "[^.!?]$" excuse)
	(setq excuse (concat excuse ".")))
    (if insert
	(insert excuse)
      (message excuse))))

;;;###autoload
(defun dup-line (&optional arg)
  "Duplicate the current line leave the point at the start of the new line.
An ARG comments out the old line."
  (interactive "P")
  (let ((line (buffer-substring (line-end-position)
				(progn (beginning-of-line) (point)))))
    ;; The above leaves the point at the start of the current line
    (if (and arg comment-start)
	(insert comment-start line comment-end "\n")
      (insert line "\n"))))

(defun mine (&optional all)
  "Return a list of all my .el files.
If ALL is non-nil, returns all .el files."
  (let ((files (directory-files-recursively user-emacs-directory ".*\\.el$")))
    (unless all
      (dolist (file files)
	(when (string-match "/elpa/\\|/ws-butler.el\\|rcfiles.el\\|custom.el\\|-loaddefs" file)
	  (setq files (delete file files)))))
    files))

;;;###autoload
(defun count-defuns (&optional all)
  (interactive "P")
  (let ((files (mine all))
	(count 0) (lines 0))
    (with-temp-buffer
      (dolist (file files)
	(insert-file-contents file))
      (goto-char (point-min))
      (while (re-search-forward "(\\(defun\\|defmacro\\|defalias\\) " nil t)
	(setq count (1+ count)))
      (goto-char (point-max))
      (setq lines (count-lines (point-min) (point-max))))
    (message "files %d lines %d count %d" (length files) lines count)))

;;;###autoload
(defun swap-in-word ()
  "Take the current word and swap it around the point.

e.g. create_thread => thread_create if the point is on the
underscore."
  (interactive)
  (let* ((ch (char-after))
	 (cur (point))
	 (start (progn (backward-word) (point)))
	 (end   (progn (forward-word)  (point)))
	 (one (buffer-substring start cur))
	 (two (buffer-substring (1+ cur) end)))
    (kill-region start end)
    (if (string-match "[a-zA-Z0-9]" (string ch))
	(insert ch two one)
      (insert two ch one))))

;;;###autoload
(defun cpuinfo ()
  "Describes the current cpus."
  (interactive)
  (let ((info (sys-cpuinfo)))
    (message "%s Vendor %s Family %d Model %d Step %d Procs %d"
	     (nth 0 info) (nth 1 info) (nth 2 info) (nth 3 info)
	     (nth 4 info) (sys-nproc))))

;;;###autoload
(defun cpuinfo-has-flag (flag &optional show)
  "Does the cpu have FLAG defined."
  (interactive "sFlag: \np")
  (let ((has-flag (member flag (sys-cpu-flags))))
    (when show
      (message "%s %s" flag (if has-flag "yes" "no")))
    has-flag))

(defun mem-human-readable (mem)
  "Convert MEM in bytes to human readable form."
  (cond
   ((> mem 1073741824)
    (format "%.1fG" (/ mem 1073741824.0)))
   ((> mem 1048576)
    (format "%.1fM" (/ mem 1048576.0)))
   (t
    (format "%.1fK" (/ mem 1024.0)))))

;;;###autoload
(defun memory ()
  "Display total and free memory."
  (interactive)
  (let ((mem (sys-mem)))
    (message "%s"
	     (concat "total " (mem-human-readable (car mem))
		     "  free " (mem-human-readable (cadr mem))
		     (when (nth 2 mem)
		       (concat "  avail " (mem-human-readable (nth 2 mem))))))))

;;;###autoload
(defun lookup-bbdb (name)
  "Lookup NAME in the bbdb.
This is a very simple version for when you don't have the bbdb
package installed."
  (interactive "sName: ")
  (let (emails full)
    (with-current-buffer (find-file-noselect "~/.bbdb")
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\[\\(nil\\|\"\\([^\"]*\\)\"\\) \"\\([^\"]*\\)\"" nil t)
	(if (equal (match-string 1) "nil")
	    (setq full (match-string 3))
	  (setq full (concat (match-string 2) " " (match-string 3))))
	(when (string-match name full)
	  (re-search-forward " (\"\\([^\"]+\\)\") ") ;; error if not found
	  (set-text-properties 0 (length full) nil full)
	  (setq emails (append emails
			       (list full (match-string-no-properties 1)))))))
    (message "%S" emails)))

;;;###autoload
(defun sys-info ()
  "System info in human readable form."
  (interactive)
  (with-output-to-temp-buffer "*sys-info*"
    (let ((os (sys-os)) (mem (sys-mem)))
      (princ (concat
	      (if (sys-is-guest) "Guest ") (car os) " " (cadr os) "\n"
	      (car (sys-cpuinfo)) " (" (number-to-string (sys-nproc)) ")\n"
	      "Memory: " (mem-human-readable (car mem))
	      "  free " (mem-human-readable (cadr mem))
	      (when (nth 2 mem)
		(concat "  avail " (mem-human-readable (nth 2 mem))))
	      "\n")))))

;;;###autoload
(defun my-toggle-case-search ()
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (message "Case sensitive search %s." (if case-fold-search "off" "on")))
