;; -*-no-byte-compile: t; -*-
;; The dynamic compile-command does not handle a byte compiled
;; qnx-make-procnto.

(defgroup qnx nil "QNX variables." :group 'tools)

(defcustom qnx-sandbox (let ((dir (getenv "QNX_SANDBOX")))
			 (if dir (file-name-as-directory dir)))
  "* Sandbox directory. Defaults to $QNX_SANDBOX. Must end in a
/!")

(defcustom qnx-build-target "smp.instr" "*Build target suffix.")

(defcustom qnx-arch-list '(("x86_64" . "o")
			   ("x86" . "o")
			   ("arm" . "le.v7")
			   ("aarch64" . "le"))
  "*List of arches and target prefixes supported by QNX.")

(defcustom qnx-make "make" "*make command. You can put flags here like make -k")

(defvar qnx-make-stages nil
  "List of make commands to perform created by `qnx-make-all'. If
a make fails, the failing command will be the car of the list.")

(defvar qnx-make-start nil "Time that `qnx-make-all' started.")

(add-to-list 'copylefts-to-hide '(" \\* \\$QNXLicenseC:" . " \\* \\$"))

(defun qnx-make-procnto (&optional arch dir)
  "Build the procnto make command."
  (unless arch (setq arch (getenv "QNX_ARCH")))
  (if dir
      (setq dir (concat dir "services/system/"))
    ;; For buffer compile commands we cannot use qnx-sandbox since it
    ;; might change.
    (if (string-match "/services/system/" buffer-file-name)
	(setq dir (substring buffer-file-name 0 (match-end 0)))
      (error "Not a procnto file %s" buffer-file-name)))
  (let ((target (assoc arch qnx-arch-list)))
    (unless target (error "Unsupported arch %s" arch))
    (concat qnx-make " -C " dir "proc/" arch "/" (cdr target) "." qnx-build-target " install")))

(defun qnx-set-arch (arch)
  "Set the QNX_ARCH environment variable from a list. Hint: M-n grabs default."
  (interactive (list (completing-read "Arch: " qnx-arch-list nil t nil nil (getenv "QNX_ARCH"))))
  (setenv "QNX_ARCH" arch))

(defun qnx-remove-stage ()
  (let ((stage (getenv "QNX_STAGE_ROOT"))
	bak)
    (unless (string-match "/home/sam/work/stage" stage)
      ;; paranoia
      (unless (yes-or-no-p (concat "Remove " stage)) (error "abort")))
    (when (file-directory-p stage)
      (setq bak (concat stage ".bak"))
      (when (file-directory-p bak)
	(delete-directory bak t))
      (rename-file stage bak))
    ))

(defun qnx-make-all ()
  "Build everything, with everything being my definition of
everything. Always builds from `qnx-sandbox', so you can call it
anywhere."
  (interactive)
  (require 'compile)
  (let* ((arch (getenv "QNX_ARCH"))
	 (qnx-make-fmt
	  (concat qnx-make " -C " qnx-sandbox "%s OSLIST=nto CPULIST=" arch " %s")))

    ;; Create the stages list
    (setq qnx-make-stages (list
			   nil ;; First entry is skipped
			   (format qnx-make-fmt "" "hinstall")
			   (format qnx-make-fmt "lib" "install")
			   (qnx-make-procnto arch qnx-sandbox)
			   (format qnx-make-fmt "hardware" "install")
			   (format qnx-make-fmt "utils" "install")))

    ;; Deal with the services directory dynamically
    (dolist (dir (directory-files (concat qnx-sandbox "services") nil "^[a-z].*"))
      (unless (equal dir "system")
	(nconc qnx-make-stages
	       (list (format qnx-make-fmt (concat "services/" dir) "install")))))

    (setq qnx-make-start (current-time))

    ;; Start by blowing away the stage directory to remove stale files
    (qnx-remove-stage)

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
    (qnx-cscope-update) ;; just a good idea
    (let ((time (cadr (time-since qnx-make-start))))
      (setq qnx-make-start nil)
      (message "Success! %d:%d" (/ time 60) (mod time 60)))))

;; Load the qnx code
(when qnx-sandbox
  (load (concat qnx-sandbox "emacs/qnx")))
