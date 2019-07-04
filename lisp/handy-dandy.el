;; Handy Dandy(tm) Functions

;;;###autoload
(defun unixtime (seconds)
  (interactive "dTime: ")
  (message (format-time-string "%a %b %d %T %Z %Y" (seconds-to-time seconds))))

;;;###autoload
(defun my-x-colour (number)
  (interactive "sColour: ")
  (cond
   ;; Convert 'd d d' to `#xxxxxx'
   ((string-match "^\\([0-9]+\\)\\([ \t]+[0-9]+\\)\\([ \t]+[0-9]+\\)$" number)
    (message "#%02x%02x%02x"
	     (string-to-number (match-string 1 number))
	     (string-to-number (match-string 2 number))
	     (string-to-number (match-string 3 number))))
   ;; Convert `#xxxxxx' to `d d d'
   ((string-match (concat "^#"
			  "\\([0-9a-fA-F][0-9a-fA-F]\\)"
			  "\\([0-9a-fA-F][0-9a-fA-F]\\)"
			  "\\([0-9a-fA-F][0-9a-fA-F]\\)$")
		  number)
    (message "%d %d %d"
	     (string-to-number (match-string 1 number) 16)
	     (string-to-number (match-string 2 number) 16)
	     (string-to-number (match-string 3 number) 16)))
   (t (error "Invalid"))))

;;;###autoload
(defun size-window (size)
  (let* ((edges (window-edges))
	 (cursize (- (nth 3 edges) (nth 1 edges) -1)))
    (unless (= size cursize)
      (enlarge-window (- size cursize)))))

(defvar signed-off-by-sig nil
  "* Signature used by `signed-off-by' function.
If nil, defaults to \"`user-full-name' <`user-mail-address'>\".")

;;;###autoload
(defun signed-off-by ()
  (interactive)
  (let ((signed-by (if signed-off-by-sig
		       signed-off-by-sig
		     (concat user-full-name " <" user-mail-address ">"))))
    (save-excursion
      (beginning-of-line)
      (insert (concat "Signed-off-by: " signed-by "\n---\n")))))

;; For when you need a good excuse...
;; From BOHF
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
      ;; misc and loaddefs not mine
      (dolist (file files)
	(when (string-match "/misc/\\|-loaddefs" file)
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
  (let* ((info (sys-cpuinfo))
	 (vendor (cadr info)))
    ;; Pretty print common vendor ids
    (cond
     ((string= "GenuineIntel" vendor) (setq vendor "Intel"))
     ((string-match "Authentic ?AMD" vendor) (setq vendor "AMD"))
     ((string= "CentaurHauls" vendor) (setq vendor "VIA")))

    (message "%s Vendor %s Family %d Model %d Step %d Procs %d"
	     (nth 0 info) vendor (nth 2 info) (nth 3 info)
	     (nth 4 info) (sys-nproc))))

;;;###autoload
(defun cpuinfo-has-flag (flag &optional show)
  "Does the cpu have FLAG defined."
  (interactive "sFlag: \np")
  (let ((has-flag (member flag (sys-cpu-flags))))
    (when show
      (message "%s %s" flag (if has-flag "yes" "no")))
    has-flag))

;; Not really cpuinfo... but makes sense here
(defun mem-human-readable (mem)
  (cond
   ((> mem 1073741824)
    (format "%.1fG" (/ mem 1073741824.0)))
   ((> mem 1048576)
    (format "%.1fM" (/ mem 1048576.0)))
   ((> mem 1024)
    (format "%.1fK" (/ mem 1024.0)))
   (t (format "%d" mem))))

;;;###autoload
(defun memory ()
  "Display total and free memory."
  (interactive)
  (let ((mem (sys-mem)))
    (message "total %s  free %s"
	     (mem-human-readable (car mem))
	     (mem-human-readable (cadr mem)))))
