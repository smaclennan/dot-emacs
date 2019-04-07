(defvar cygwin-dir nil
  "I recommend setting cygwin-dir in user-init.el.")

(when cygwin-dir
  (add-to-list 'exec-path (concat cygwin-dir "bin")) t)

;; NT uses findstr
(set-variable 'grep-command "findstr -n ")
(setq my-grep-prog "findstr -n ")

;; -------------------------------------------------------------
;; flyspell - you may need to hardcode the path
;; Falls back to a batch file which claims every word is correct
(setq ispell-program-name
      (or (executable-find "ispell")
	  (executable-find "aspell")
	  (concat user-emacs-directory "src/ispell.bat")))

;; Different excludes for windows
(setq smerge-diff-excludes
      '("*.obj" "*.lib" "*.dll" "*.sbr" ".svn" "*.scc"
	"*.plg" "*.pdb" "*.dep" "*.ncb" "*.opt"
	"*.log" "*.wrn" "*.err"
	"*.mak"
	"objchk*" "objfre*"
	"debug" "release" "Debug" "Release"))

(eval-after-load "smerge"
  `(defun smerge-fixup-filenames ()
     "Diff splits the `Only in' files into directory and filename.
Top level directories end in /, subdirs do not. Windows version."
     (goto-char (point-min))
     (while (re-search-forward "\\(.\\): " nil t)
       (if (eq (string-to-char (match-string 1)) ?/)
	   (replace-match "/") (replace-match "\\1/")))))

(eval-after-load "smerge"
  `(defun smerge-lists ()
     "Create strings for only-in-1, only-in-2, both. Windows version."
     (list (format "^Only in %s:? *\\(.*\\)$" (regexp-quote smerge-dir1))
	   (format "^Only in %s:? *\\(.*\\)$" (regexp-quote smerge-dir2))
	   (format "^Files %s\\(.+\\) and %s.+ differ$"
		   (regexp-quote smerge-dir1)
		   (regexp-quote smerge-dir2)))))

(defun sys-nproc () (string-to-number (getenv "NUMBER_OF_PROCESSORS")))

(defun sys-cpuinfo ()
  (let ((ident (getenv "PROCESSOR_IDENTIFIER")))
    (unless (string-match (concat "^[^ ]+ Family \\([0-9]+\\) "
				  "Model \\([0-9]+\\) "
				  "Stepping \\([0-9]+\\), "
				  "\\(.*\\)$") ident)
      (error "Vendor not found in %s" ident))
    (list (match-string 4 ident)
	  (string-to-number (match-string 1 ident))
	  (string-to-number (match-string 2 ident))
	  (string-to-number (match-string 3 ident)))))

(defun sys-mb (str)
  "Helper function to convert number with commas from MB to bytes."
  (let ((mb (string-to-number (replace-regexp-in-string "," "" str))))
    (* mb #x100000)))

(defun sys-mem ()
  "Return the total and free memory reported by systeminfo.
WARNING: This can take over 10 seconds."
  (let (total avail)
    (with-temp-buffer
      (message "Calling systeminfo... Please wait...")
      (call-process "systeminfo" nil t)
      (message "Calling systeminfo... done.")
      (goto-char (point-min))
      (re-search-forward "^Total Physical Memory: *\\([0-9,]+\\) MB")
      (setq total (sys-mb (match-string 1)))
      (re-search-forward "^Available Physical Memory: *\\([0-9,]+\\) MB")
      (setq avail (sys-mb (match-string 1))))
    (list total avail)))

(defun build-all-loaddefs ()
  (interactive)
  (dolist (one '("lisp" "misc"))
    (let* ((dir (concat user-emacs-directory one))
	   (generated-autoload-file
	    (concat dir "/" one "-loaddefs.el")))
      (update-directory-autoloads dir))))
