(defgroup qnx nil "QNX variables." :group 'tools)

(defcustom qnx-sandbox (getenv "QNX_SANDBOX")
  "* Sandbox directory. Defaults to QNX_SANDBOX environment
  variable if set. Must end in /!")

(defcustom qnx-build-arch  "x86_64" "*Arch to build.")
(defcustom qnx-build-target "smp.instr" "*Build target suffix.")

(defcustom qnx-arch-list '(("x86_64" . "o.")
			   ("x86" . "o.")
			   ("arm" . "le.v7.")
			   ("aarch64" . "le"))
  "*List of arches and target prefixes supported by QNX.")

(defun qnx-make-procnto (arch)
  "Given ARCH, use `qnx-sandbox' and `qnx-build-target' to build
the procnto make command."
  (let ((target (assoc arch qnx-arch-list)))
    (unless target (error "Unsupported arch %s" arch))
    (concat "make -C " qnx-sandbox "services/system/proc/"
	    arch "/" (cdr target) qnx-build-target " install")))

(defun qnx-func (matched-dir target)
  (if (equal target "procnto")
      (setq compile-command (qnx-make-procnto qnx-build-arch))
    (setq compile-command
	  (concat "make -C " matched-dir " OSLIST=nto CPULIST=" qnx-build-arch " install")))

  (when (equal target "utils")
    (indent-N 4 nil)) ;; 4 char spaces

  (set (make-local-variable 'make-clean-command)
       (concat "make -C " matched-dir " clean"))

  (defun qnx-cscope-update ()
    (shell-command "find -name unittests -prune -o -name '*.[ch]' -print > cscope.files"))

  (set (make-local-variable 'cscope-initial-directory) qnx-sandbox)
  (set (make-local-variable 'cscope-update-args) "-q -b -k -i cscope.files")
  (add-hook 'cscope-update-hooks 'qnx-cscope-update t t)
  )
;; qnx-func

(defun work-init ()
  (add-to-list 'my-compile-dir-list '(".*/unittests/.*") t)
  (add-to-list 'my-compile-dir-list (list (concat qnx-sandbox "lib/") "lib" 'qnx-func) t)
  (add-to-list 'my-compile-dir-list (list (concat qnx-sandbox "hardware/") "hw" 'qnx-func) t)
  (add-to-list 'my-compile-dir-list (list (concat qnx-sandbox "utils/") "utils" 'qnx-func) t)
  (add-to-list 'my-compile-dir-list (list (concat qnx-sandbox "services/") "procnto" 'qnx-func) t)
  )
(add-hook 'my-compile-init-hooks 'work-init)

(setq ogrok-url "http://10.222.97.117:8080/source"
      ogrok-project "product_mainline"
      ogrok-xref "/source/xref/product_mainline/"
      ogrok-path "%21unittests"
      ogrok-base qnx-sandbox)

(defvar kloc-project-dir ".kwlp_mainline_procnto_full"
  "*Klocwork sub-project directory to use.")

(setq kloc-dir (concat qnx-sandbox kloc-project-dir "/"))

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

(defun delta (time)
  (setq time (cadr time))
  (let ((sec (mod time 60))
	(min (/ time 60)))
    (format "%d:%d" min sec)))

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
