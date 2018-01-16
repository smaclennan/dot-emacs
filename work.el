(defgroup qnx nil "QNX variables." :group 'tools)

(defcustom qnx-sandbox (getenv "QNX_SANDBOX")
  "* Sandbox directory. Defaults to QNX_SANDBOX environment
variable if set. Must end in /!")

(defcustom qnx-build-arch  "x86_64" "*Arch to build.")
(defcustom qnx-build-target "smp.instr" "*Build target suffix.")

(defcustom qnx-arch-list '(("x86_64" . "o")
			   ("x86" . "o")
			   ("arm" . "le.v7")
			   ("aarch64" . "le"))
  "*List of arches and target prefixes supported by QNX.")

(defun qnx-make-procnto (arch)
  "Given ARCH, use `qnx-sandbox' and `qnx-build-target' to build
the procnto make command."
  (let ((target (assoc arch qnx-arch-list)))
    (unless target (error "Unsupported arch %s" arch))
    (concat "make -C " qnx-sandbox "services/system/proc/"
	    arch "/" (cdr target) "." qnx-build-target " install")))

;;;###autoload
(defun qnx-reset-arch ()
  "Reset the current compile command for the current buffer to
the current `qnx-build-arch'."
  (interactive)
  (if (string-match "CPULIST=[^ ]+" compile-command)
      (setq compile-command
	    (replace-match (concat "CPULIST=" qnx-build-arch) nil nil compile-command))
    (setq compile-command (qnx-make-procnto qnx-build-arch)))
  (message compile-command))

(defun qnx-files-update ()
  (shell-command (concat "find -name unittests -prune "
			 "-o -type l -prune "
			 "-o -name '*.[ch]' -print > cscope.files")))

(defun qnx-cscope-update ()
  (interactive)
  (let ((default-directory qnx-sandbox))
    (qnx-files-update)
    (shell-command "cscope -q -k -b")))

(defun qnx-tags-update ()
  (interactive)
  (let ((default-directory qnx-sandbox))
    ;; reuse cscope.files
    (qnx-files-update)
    (shell-command "cat cscope.files | etags -")))

(defun qnx-func (matched-dir target)
  (if (equal target "procnto")
      (setq compile-command (qnx-make-procnto qnx-build-arch))
    (setq compile-command
	  (concat "make -C " matched-dir " OSLIST=nto CPULIST=" qnx-build-arch " install")))

  (when (equal target "utils")
    (indent-N 4 nil)) ;; 4 char spaces

  (set (make-local-variable 'make-clean-command)
       (concat "make -C " matched-dir " clean"))

  (set (make-local-variable 'my-cscope-args) "-q -k")

  ;; (my-tags-update-helper qnx-sandbox)

  ;; Try to pick a reasonable project - not complete
  (let ((proj))
    (cond
     ((equal target "procnto") (setq proj ".kwlp_mainline_procnto_full"))
     ((equal target "lib")     (setq proj ".kwlp_mainline_libc_full"))
     ((equal target "utils")   (setq proj ".kwlp_x86_hypervisor"))
     )
    (when proj
      (set (make-local-variable 'kloc-dir) (concat qnx-sandbox proj))))

  (setq mode-name (concat "qnx-" mode-name)))
;; qnx-func

(defun gdb-func (matched-dir target)
  "GDB the Ubuntu way"
  (setq compile-command (concat "make " make-j " -C " matched-dir "/build/objdir"))
  (c-set-style "gnu")
  (setq c-basic-offset 2)
  (setq tab-width 8))

(defun work-init ()
  (add-to-list 'my-compile-dir-list '(".*/unittests/.*") t)
  (add-to-list 'my-compile-dir-list (list (concat qnx-sandbox "lib/") "lib" 'qnx-func) t)
  (add-to-list 'my-compile-dir-list (list (concat qnx-sandbox "hardware/[^/]+/[^/]+/") "hw" 'qnx-func) t)
  (add-to-list 'my-compile-dir-list (list (concat qnx-sandbox "utils/[a-z]/[^/]+/") "utils" 'qnx-func) t)
  (add-to-list 'my-compile-dir-list (list (concat qnx-sandbox "services/system/") "procnto" 'qnx-func) t)
  (add-to-list 'my-compile-dir-list (list (concat qnx-sandbox "services/[^/]+/") "service" 'qnx-func) t)

  (add-to-list 'my-compile-dir-list (list "^.*/gdb-[0-9.]+/" nil 'gdb-func) t)
  )

;; If qnx-sandbox is nil, these configs will mess up
(when qnx-sandbox
  (add-hook 'my-compile-init-hooks 'work-init)

  ;; At work cscope is more useful than tags... but memory muscle can
  ;; be a terrible thing.
  (global-set-key [f10]   'my-cscope-at-point)
  (global-set-key [?\M-.] 'my-cscope)

  (when (not running-xemacs)
    (setq frame-title-format '("" emacs-str
			       (buffer-file-name
				(:eval
				 (if (eq (cl-search qnx-sandbox buffer-file-name) 0)
				     (concat "~qnx/" (substring buffer-file-name (length qnx-sandbox)))
				   (abbreviate-file-name buffer-file-name)))
				"%b"))))
  )

;;; --------- make all

;; For qnx-make-all, -j8 didn't help that much

(defvar qnx-make-stages nil
  "List of make commands to perform created by `qnx-make-all'. If
a make fails, the failing command will be the car of the list.")

(defvar qnx-make-start nil "Time that `qnx-make-all' started.")

;;;###autoload
(defun qnx-set-arch (arch)
  "Set the `qnx-build-arch' variable from a list of arches."
  (interactive (list
		(completing-read "Arch: " qnx-arch-list nil t)))
  (setq qnx-build-arch arch))

;;;###autoload
(defun qnx-make-all (arg)
  "Build everything, with everything being my definition of
everything. If ARG is non-nil, then prompt for the arch to build
for. The default is `qnx-build-arch'."
  (interactive "P")

  (let (qnx-make-fmt
	(arch (if arg
		  (completing-read "Arch: " qnx-arch-list nil t)
		qnx-build-arch)))

    (setq qnx-make-fmt
	  (concat "make -C " qnx-sandbox "%s OSLIST=nto CPULIST=" arch " %s"))

    ;; Create the stages list
    (setq qnx-make-stages (list
			   nil ;; First entry is skipped
			   (format qnx-make-fmt "" "hinstall")
			   (format qnx-make-fmt "lib" "install")
			   (qnx-make-procnto arch)
			   (format qnx-make-fmt "hardware" "install")
			   (format qnx-make-fmt "utils" "install")))

    ;; Deal with the services directory dynamically
    (dolist (dir (directory-files (concat qnx-sandbox "services") nil "^[a-z].*"))
      (unless (equal dir "system")
	(nconc qnx-make-stages (list (format qnx-make-fmt (concat "services/" dir) "install")))))

    (setq qnx-make-start (current-time))

    ;; Start if off by pretending to successfully finish a stage
    (add-hook 'compilation-finish-functions 'qnx-make-finish)
    (qnx-make-finish nil "finished\n")
    ))

(defun qnx-make-finish (buffer desc)
  (unless (equal desc "finished\n")
    (remove-hook 'compilation-finish-functions 'qnx-make-finish)
    (error "Stage: %s" desc))
  (setq qnx-make-stages (cdr qnx-make-stages)) ;; next
  (if qnx-make-stages
      ;; The let binding is here to stop `compile' from setting the global var
      (let ((compile-command (car qnx-make-stages)))
	(message "stage %s..." compile-command)
	(compile compile-command))
    ;; Done!
    (remove-hook 'compilation-finish-functions 'qnx-make-finish)
    (let ((time (cadr (time-since qnx-make-start))))
      (setq qnx-make-start nil)
      (message "Success! %d:%d" (/ time 60) (mod time 60)))))
