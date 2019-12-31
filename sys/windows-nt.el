;; If you have cygwin installed, add something like the following to
;; user-init.el
;; (add-to-list 'exec-path "<cygwin-dir>/bin" t)

;; If you have aspell or ispell installed, add that to user-init.el
;; and comment out the line here.
(setq ispell-program-name (concat user-emacs-directory "src/ispell.bat"))

;; NT uses findstr
(set-variable 'grep-command "findstr -n ")
(setq my-grep-prog "findstr -n ")

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

(defvar sys-nproc nil "Total number of processors.")

(defvar sys-mem nil "Total system memory.")

;;;###autoload
(defun sys-os ()
  ;; I'm sure there is a way to get the version, but a quick search
  ;; only found the About Box.
  '("Windows" "unknown"))

;;;###autoload
(defun sys-nproc ()
  (string-to-number (getenv "NUMBER_OF_PROCESSORS")))

;;;###autoload
(defun sys-cpuinfo ()
  (let ((ident (getenv "PROCESSOR_IDENTIFIER")))
    (unless (string-match (concat "^\\([^ ]+\\) Family \\([0-9]+\\) "
				  "Model \\([0-9]+\\) "
				  "Stepping \\([0-9]+\\), "
				  "\\(.*\\)$") ident)
      (error "Vendor not found in %s" ident))
    (list (match-string 1 ident) ;; not really model name
	  (match-string 5 ident)
	  (string-to-number (match-string 2 ident))
	  (string-to-number (match-string 3 ident))
	  (string-to-number (match-string 4 ident)))))

;;;###autoload
(defun sys-cpu-flags ()
  (load "cpuid")
  (cpuid-cpu-flags))

;;;###autoload
(defun sys-is-guest ()
  (load "cpuid")
  (cpuid-is-guest))

(defun sys-mb (str)
  "Helper function to convert number with commas from MB to bytes."
  (let ((mb (string-to-number (replace-regexp-in-string "," "" str))))
    (* mb #x100000)))

;;;###autoload
(defun sys-mem ()
  "Return the total and free memory reported by systeminfo.
WARNING: This can take over 10 seconds."
  (let (avail)
    (with-temp-buffer
      (message "Calling systeminfo... Please wait...")
      (call-process "systeminfo" nil t)
      (message "Calling systeminfo... done.")
      (goto-char (point-min))
      (re-search-forward "^Total Physical Memory: *\\([0-9,]+\\) MB")
      (setq sys-mem (sys-mb (match-string 1)))
      (re-search-forward "^Available Physical Memory: *\\([0-9,]+\\) MB")
      (setq avail (sys-mb (match-string 1))))
    (list sys-mem avail)))

(defun build-all-loaddefs ()
  (interactive)
  (let* ((dir (concat user-emacs-directory "lisp"))
	 (generated-autoload-file
	  (concat dir "/" "lisp" "-loaddefs.el")))
    (update-directory-autoloads dir)))
