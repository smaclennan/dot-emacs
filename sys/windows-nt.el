;; NT uses findstr
(set-variable 'grep-command "findstr -n ")
(setq my-grep-prog "findstr -n ")

;; -----------------------------------------------------------
;; aspell for nt
(let ((spell-prog (or (executable-find "aspell")
		      (executable-find "aspell"))))
  (when spell-prog
    (require 'ispell)
    (setq ispell-program-name spell-prog)
    (setq ispell-extra-args '("--reverse"))))

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
