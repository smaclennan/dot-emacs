;; -*-no-byte-compile: t; -*-
;; The dynamic compile-command does not handle a byte compiled
;; qnx-make-procnto.

(defgroup qnx nil "QNX variables." :group 'tools)

(defcustom qnx-sandbox (let ((dir (getenv "QNX_SANDBOX")))
			 (if dir (file-name-as-directory dir)))
  "* Sandbox directory. Defaults to $QNX_SANDBOX. Must end in a
/!")

(defcustom qnx-build-arch  "x86_64" "*Arch to build.")
(defcustom qnx-build-target "smp.instr" "*Build target suffix.")

(defcustom qnx-arch-list '(("x86_64" . "o")
			   ("x86" . "o")
			   ("arm" . "le.v7")
			   ("aarch64" . "le"))
  "*List of arches and target prefixes supported by QNX.")

(add-to-list 'copylefts-to-hide '(" \\* \\$QNXLicenseC:" . " \\* \\$"))

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
    (concat "make -C " dir "proc/" arch "/" (cdr target) "." qnx-build-target " install")))

(defun qnx-func (matched-dir target)
  (cond
   ((equal target "procnto")
    (setq compile-command '(qnx-make-procnto)))
   ((equal target "unittests")
    (setq compile-command "make "))
   (t ;; This cannot be dynamic because of matched-dir.
    (setq compile-command
	  (concat "make -C " matched-dir " OSLIST=nto CPULIST=$QNX_ARCH install"))))

  (set (make-local-variable 'make-clean-command)
       (concat "make -C " matched-dir " clean"))

  ;; SAM (set (make-local-variable 'my-cscope-args) "-q -k")

  ;; (my-tags-update-helper qnx-sandbox)

  ;; Try to pick a reasonable project - not complete
  (let ((proj))
    (cond
     ((equal target "procnto") (setq proj ".kwlp_mainline_procnto_full"))
     ((equal target "lib")     (setq proj ".kwlp_mainline_libc_full"))
     ((equal target "utils")   (setq proj ".kwlp_x86_hypervisor")
      (indent-N 4 nil)) ;; 4 char spaces
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
  (nconc my-compile-dir-list
	 (list
	  (list (concat qnx-sandbox ".*/unittests/") "unittests" 'qnx-func)
	  (list (concat qnx-sandbox "lib/[^/]+/") "lib" 'qnx-func)
	  (list (concat qnx-sandbox "hardware/[^/]+/[^/]+/") "hw" 'qnx-func)
	  (list (concat qnx-sandbox "utils/[a-z]/[^/]+/") "utils" 'qnx-func)
	  (list (concat qnx-sandbox "services/system/") "procnto" 'qnx-func)
	  (list (concat qnx-sandbox "services/[^/]+/") "service" 'qnx-func)

	  (list "^.*/gdb-[0-9.]+/" nil 'gdb-func)))

  ;; At work cscope is more useful than tags... but memory muscle can
  ;; be a terrible thing.
  (global-set-key [f10]   'my-cscope-at-point)
  (global-set-key [?\M-.] 'my-cscope)

  ;; Let's try always enabling cscope
  ;; Let's also try -d and see how that goes...
  (setq my-cscope-dir qnx-sandbox
	my-cscope-args "-q -k -d")

  (when (not running-xemacs)
    (setq frame-title-format
	  '("" emacs-str
	    (buffer-file-name
	     (:eval
	      (if (eq (cl-search qnx-sandbox buffer-file-name) 0)
		  (concat "~qnx/" (substring buffer-file-name (length qnx-sandbox)))
		(abbreviate-file-name buffer-file-name)))
	     "%b"))))
  )

;; If qnx-sandbox is nil, these configs will mess up
(when qnx-sandbox (add-hook 'my-compile-init-hooks 'work-init))

(defun qnx-unittest (func)
  (interactive "sFunc: ")
  ;; Create the file if necessary
  (find-file (concat "./" func ".c"))
  ;; Fill in the stub code
  (let (here)
    (insert "#include \"unittest.h\"\n\n")
    ;; (insert "#include \"source.c\"\n\n")
    (insert "#include \"common.c\"\n\n")
    (insert "int main(void)\n{\n")
    (insert "\t")
    (setq here (point))
    (insert func "();\n")
    (insert "\treturn UT_PASS;\n}\n")
    (goto-char here)
    )
  ;; Add to Makefile
  (with-current-buffer (find-file-noselect "Makefile")
    (goto-char (point-min))
    (unless (re-search-forward "^TESTLIST")
      (error "No TESTLIST in Makefile"))
    (end-of-line) (forward-char)
    (while (looking-at "^[::blank::][a-z]")
      (end-of-line) (forward-char))
    (insert (concat "\t" func " \\\n")))
  )

;;; --------- make all

;; For qnx-make-all, -j8 didn't help that much

(defvar qnx-make-stages nil
  "List of make commands to perform created by `qnx-make-all'. If
a make fails, the failing command will be the car of the list.")

(defvar qnx-make-start nil "Time that `qnx-make-all' started.")

(defun qnx-set-arch (arch)
  "Set the QNX_ARCH environment variable from a list. Hint: M-n grabs default."
  (interactive (list (completing-read "Arch: " qnx-arch-list nil t nil nil (getenv "QNX_ARCH"))))
  (setenv "QNX_ARCH" arch))

(defun qnx-make-all ()
  "Build everything, with everything being my definition of
everything. Always builds from `qnx-sandbox', so you can call it
anywhere."
  (interactive)
  (require 'compile)
  (let* ((arch (getenv "QNX_ARCH"))
	 (qnx-make-fmt
	  (concat "make -C " qnx-sandbox "%s OSLIST=nto CPULIST=" arch " %s")))

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
