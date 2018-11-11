;; The rc files are called when the correspoding mode is loaded.
;; Because of this, it is very hard to compile them from outside the
;; editor (e.g. from a Makefile) since they need a fully running
;; environment. So the solution is to compile them from within emacs.

;;;###autoload
(defun rc-compile (rcfile)
  "Compile one rc file."
  (interactive "frcfile: ")
  (let (mode)
    (unless (string-match "^\\(.*\\)-rc\.el$" rcfile)
      (error "%s not a proper rcfile" rcfile))

    ;; We must make sure the appropriate mode is loaded before trying
    ;; to compile or we will get bogus warnings.
    (setq mode (file-name-nondirectory (match-string 1 rcfile)))
    (unless (load mode)
      (error "Could not load mode %s" mode))

    ;; Compile the file
    (byte-compile-file rcfile)
    ))

;;;###autoload
(defun rc-compile-all ()
  "Compile all the files in the rc directory.
Warning: Erases the compile log output buffer."
  (interactive)

  ;; This is a bit evil - delete the old logs
  (when (get-buffer "*Compile-Log*")
    (kill-buffer "*Compile-Log*"))

  (let ((files (directory-files (concat dot-dir "rc") t ".*-rc\.el$"))
	(winconf (concat dot-dir "rc/window-config.el")))
    (if (featurep 'emacs)
	;; Emacs does not have efs or package-ui
	(progn
	  (setq files (delete (concat dot-dir "rc/efs-rc.el") files))
	  (setq files (delete (concat dot-dir "rc/ksh-mode-rc.el") files))
	  (setq files (delete (concat dot-dir "rc/package-ui-rc.el") files))
	  (setq files (delete (concat dot-dir "rc/python-mode-rc.el") files))
	  (setq files (delete (concat dot-dir "rc/xcscope-rc.el") files)))
      (setq files (delete (concat dot-dir "rc/python-rc.el") files)))
    (dolist (rcfile files) (rc-compile rcfile))

    ;; windows-config.el is special
    (and window-system
	 (file-exists-p winconf)
	 (byte-compile-file winconf))
  ))

(provide 'rc-compile)