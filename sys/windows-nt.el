;;; Emacs windoze setup -*- Mode:emacs-lisp -*-

;; For Windoze, the HOME and DOMAINNAME environment variables *must* be set

(if (string= user-full-name "")
    (if (string= (user-login-name) "sam")
	(setq user-full-name "Sean MacLennan")
      (setq user-full-name (user-login-name))))

(setq directory-sep-char ?/)

(defadvice call-process (around windowize activate)
  (let ((directory-sep-char ?\\))
    ad-do-it
    ))

;; For cmaxecp and others
(defadvice call-process-region (around windowize activate)
  (let ((directory-sep-char ?\\))
    ad-do-it
    ))

;; For compile
(defadvice start-process-shell-command (around windowize activate)
  (let ((directory-sep-char ?\\))
    ad-do-it
    ))

(defvar vs-tools-dir "c:/Program Files (x86)/Microsoft Visual Studio/2017/Community/Common7/Tools/")

(defun emacs-version>= (major minor)
  (or (> emacs-major-version major)
      (and (eq emacs-major-version major)
	   (>= emacs-minor-version minor))))

(when nil ;; SAM not working
(if (file-exists-p vs-tools-dir)
    (progn
      (shell-command (concat "\"" vs-tools-dir "VsMsBuildCmd.bat\""))
      (shell-command (concat "\"" vs-tools-dir "vsdevcmd/ext/vcvars.bat\""))
      )
  (message "Can't find %s" vs-tools-dir))
)

(setq find-file-compare-truenames t)

;; NT uses nmake and findstr
(set-variable 'grep-command "findstr -n ")
(setq compile-command "nmake ")
(setq make-clean-command "nmake clean all")

(setq local-compile-command "cl /Ot /W3")

;; SAM SAM SAM
;; For windoze only hashing seems to work with auto-save
;; SAM (setq auto-save-directory (win32-short-file-name auto-save-directory))
(unless (emacs-version>= 21 4)
  (setq auto-save-hash-p t
	backup-hash-p t
	auto-save-hash-directory (concat auto-save-directory "hash/")))

(when nil
(require 'backup-dir)
(setq bkup-backup-directory-info
      (cons (list 't (expand-file-name "~/backup") 'full-path 'prepend-name 'ok-create)
	    '()))
)

;; Strip down the error regexp for speed.
(setq compilation-error-regexp-systems-list '(msft))

;; SAM This binding is broken in later betas
(if (eq (key-binding [(shift insert)]) 'x-yank-clipboard-selection)
    (global-set-key [(shift insert)] 'mswindows-paste-clipboard))


;;--------------------------------------------------------------
(unless (emacs-version>= 21 4) ;; SAM
  ;; This is defined wrong in win32-native.el
  (defun make-auto-save-file-name (&optional file-name)
    "Return file name to use for auto-saves of current buffer.
Does not consider `auto-save-visited-file-name' as that variable is checked
before calling this function.  You can redefine this for customization.
See also `auto-save-file-name-p'."
    (let ((name (original-make-auto-save-file-name file-name))
	  (start 0))
      ;; destructively replace occurrences of * or ? with $
      (while (string-match "[?*]" name start)
	(aset name (match-beginning 0) ?$)
	(setq start (1+ (match-end 0))))
      name))
  )

;;--------------------------------------------------------------
;; Position the frames
(setq initial-frame-plist '(top 1 left 486 width 90 height 59)
      default-frame-plist '(top 1 left   1 width 90 height 59))

;; -----------------------------------------------------------
;; aspell for nt - will not work with ispell.el 3.3 or 3.4
(require 'ispell)
(let ((aspell-prog "c:/Program Files/aspell/bin/aspell.exe"))
  (when (file-exists-p aspell-prog)
    (setq ispell-program-name aspell-prog)))
;;(setq ispell-extra-args '("--conf=d:/aspell/aspell.conf" "--dict-dir=d:/aspell/dict"))
(when (string-match "3.0" ispell-version)
  (setq ispell-extra-args '("--reverse")))

;; Different excludes for windows
(setq smerge-diff-excludes
      '("*.obj" "*.lib" "*.dll" "*.sbr" ".svn" "*.scc"
	"*.plg" "*.pdb" "*.dep" "*.ncb" "*.opt"
	"*.log" "*.wrn" "*.err"
	"*.mak"
	"objchk*" "objfre*"
	"debug" "release" "Debug" "Release"))

(defun sys-nproc () (string-to-number (getenv "NUMBER_OF_PROCESSORS")))
