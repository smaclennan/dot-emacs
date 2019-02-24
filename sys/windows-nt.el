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

(defun build-all-loaddefs ()
  (interactive)
  (dolist (one '("lisp" "misc"))
    (let* ((dir (concat user-emacs-directory one))
	   (generated-autoload-file
	    (concat dir "/" one "-loaddefs.el")))
      (update-directory-autoloads dir))))
