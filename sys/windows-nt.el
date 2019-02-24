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

(defun build-all-loaddefs ()
  (interactive)
  (dolist (one '("lisp" "misc"))
    (let* ((dir (concat user-emacs-directory one))
	   (generated-autoload-file
	    (concat dir "/" one "-loaddefs.el")))
      (update-directory-autoloads dir))))
