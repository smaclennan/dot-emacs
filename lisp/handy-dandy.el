;; Handy Dandy(tm) Functions

;;;###autoload
(defun unixtime (seconds)
  "Convert SECONDS in Unix time to human readable date."
  (interactive "nTime: ")
  (message (format-time-string "%a %b %d %T %Z %Y" (seconds-to-time seconds))))

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

(defvar mine-not
  (concat "/elpa/\\|/ws-butler.el\\|rcfiles.el\\|custom.el\\|"
	  "-loaddefs\\|view-kill.el\\|fill-column-indicator.el"))

(defun mine (&optional all)
  "Return a list of all my .el files.
If ALL is non-nil, returns all .el files."
  (let ((files (directory-files-recursively user-emacs-directory ".*\\.el$")))
    (unless all
      (dolist (file files)
	(when (string-match mine-not file)
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
(defun my-toggle-case-search ()
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (message "Case sensitive search %s." (if case-fold-search "off" "on")))

;;;###autoload
(defun time-diff (start &optional end)
  "Diff two time strings and print the result.
If END is nil, or an empty string, then the current time is used.
Hint: The output from the Unix date command works."
  (interactive "sStart: \nsEnd: ")
  (when (stringp end)
    (if (equal end "")
	(setq end nil)
    (setq end (date-to-time end))))
  (let ((diff (time-subtract end (date-to-time start)))
	hours days weeks)
    (when (listp diff)
      (setq diff (+ (ash (car diff) 16) (cadr diff))))
    (setq hours (/ diff 3600))
    (setq days (/ hours 24))
    (setq weeks (/ days 7))
    (if (> weeks 1)
	(message "%d weeks %d days %d hours" weeks (% days 7) (% hours 24))
      (if (> days 1)
	  (message "%d days %d hours" days (% hours 24))
	(message "%d hours" hours)))))

;;;###autoload
(defun cat-years (age)
  (interactive "nAge in years: ")
  (if (>= age 1) (setq age (+ age 15))) ;; first year 16
  (if (> age 16) (setq age (+ age 4)))  ;; second year 5
  (if (> age 21) (setq age (+ age (* (- age 21) 3)))) ;; other years 4
  (message "Cat years %d" age))
