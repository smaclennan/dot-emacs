;; Handy Dandy(tm) Functions

(require 'sam-common)
(require 'dirfiles)

;;;###autoload
(defun unixtime (seconds)
  (interactive "sTime: ")
  ;; Force it to a float for 32-bit systems.
  (let ((time (string-to-number (concat seconds ".0"))))
    (message "%s"
	     (format-time-string
	      "%a %b %d %T %Z %Y"
	      ;; seconds-to-time from time-date.el in gnus
	      (list (floor time 65536)
		    (floor (mod time 65536))
		    (floor (* (- time (ffloor time)) 1000000)))
	      ))))

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
			  "\\([0-9a-fA-F][0-9a-fA-F]\\)$") number)
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

;;;###autoload
(defun load-path-roots ()
  "Show only the root dirs in the `load-path'."
  (interactive)
  (let (dirs)
    (dolist (dir load-path)
      (when (string-match "/lisp/.*" dir)
	(setq dir (replace-match "" nil nil dir)))
      (add-to-list 'dirs dir))
    (if (my-interactive-p) (message "%S" dirs))
    dirs))

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
  "Return a list of all my .el files. If `all' is non-nil,
returns all .el files."
  (let* ((base (file-name-directory user-init-file))
	 (files (directory-files-recursive base ".*\.el$")))
    (unless all
      ;; misc not mine
      (dolist (file files)
	(when (string-match ".*/misc/.*" file)
	  (setq files (delete file files))))

      (dolist (file '(;; generated files
		      "lisp/lisp-loaddefs.el" "misc/misc-loaddefs.el"
		      ;; ignore this to make comparisons
		      "user-init.el"
		      ))
	(setq files (delete (concat base file) files))))
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
(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))
