(defvar pika-dir (getenv "PIKA_DIR")
  "* Root of the PIKA source tree.
Use `set-pika-dir' to change this.")

(defvar pika-kernel (getenv "PIKA_KERNEL")
  "* Set this to compile for a different kernel version.
Nil defaults to the currently running kernel.")

(defvar pika-cflags (getenv "PIKA_CFLAGS")
  "* Set any extra kernel flags you want to pass to user mode.")

(defvar pika-kernel-cflags (getenv "PIKA_KERNEL_CFLAGS")
  "* Set any extra kernel flags you want to pass to all the kernel drivers.")

(defvar pika-subdirs '("user"
		       "kernel"
		       "applications/aohtest" "applications"
		       )
  "* Pika sub-directories to handle specially.")

(eval-when-compile (would-like 'ppc-env))

(if nil
    ;; ELDK
    (setq ppc-toolchain-dir "/usr/src/eldk")
  ;; PADS
  (setq ppc-toolchain-dir (expand-file-name "~work/pads/toolchain")))

(defvar pika-gp-dir (or (getenv "PIKA_GP_DIR") "~work/grandprix/software")
  "* Root of the PIKA Grandprix source tree.")

;; -------------------------------------------------------------------
;; Some evil troll decided that code should be unreadable and declared
;; tabs to be 2 characters. To make matters worse, they decided they
;; must be spaces so we cannot adjust the spacing ourselves.....

(defun pika-c-initialization-hook ()
  (c-add-style "pika" '("linux" (c-basic-offset . 2))))
(add-hook 'c-initialization-hook 'pika-c-initialization-hook)

;; When called from my-compile-command is passed two args.
;; When called from c-mode-common-hook gets none.
(defun pika-c-mode (&optional dir arg)
  (interactive)
  (when pika-kernel
    (setq compile-command
	  (format "%s KERNEL=%s" compile-command pika-kernel)))
  (if (and dir (string-match "/kernel/" dir))
      (progn
	(c-set-style "linux")
	(setq mode-name (concat "PK-" mode-name))
	(setq indent-tabs-mode t)
	(setq tab-width 8))
    (c-set-style "pika")
    (setq mode-name (concat "PIKA-" mode-name))
    (setq indent-tabs-mode nil)
    (setq tab-width 2)
    )
  (pika-monza-lxr)
  (force-mode-line-update))

;; For windows assume we are always editing pika code
(when running-windoze
  (add-hook 'c-mode-common-hook 'pika-c-mode 'append))

(defun pika-monza-lxr ()
  (setq lxr-url "http://alice.pikatech.com/lxr/http/"
	lxr-base pika-dir
	lxr-version "git-monza"
	lxr-arch nil
	))


;; -------------------------------------------------------------------
(defvar default-compile-dirs nil "Local var. Do not touch.")

;; Handy little function.
(defsubst pika-compile-dir-append (regexp)
  (setq my-compile-dir-list
	(append my-compile-dir-list
		(list (list regexp nil 'pika-c-mode)))))

(defun set-pika-dir (&optional dir)
  (interactive)
  (when (interactive-p)
    (setq dir (read-directory-name "pika-dir: " pika-dir pika-dir)))
  (setq dir (my-expand-dir-name dir))
  (let ((maybe-dir (concat dir "/software")))
    (if (file-directory-p dir)
	(if (file-directory-p maybe-dir)
	  (if (y-or-n-p (concat "Do you want " maybe-dir "? "))
	      (setq dir maybe-dir)))
      (error "%s not a directory." dir)))

  (setq pika-dir dir)
  (setenv "PIKA_DIR" pika-dir)

  ;; Note: if these are nil, setenv will remove them
  (setenv "PIKA_KERNEL" pika-kernel)
  (setenv "PIKA_CFLAGS" pika-cflags)
  (setenv "PIKA_KERNEL_CFLAGS" pika-kernel-cflags)

  (when (eq (getenv "LD_LIBRARY_PATH") nil)
    (setenv "LD_LIBRARY_PATH" (concat pika-dir "/user/libs")))

  ;; Save them the first time
  (unless default-compile-dirs
    (setq default-compile-dirs my-compile-dir-list))

  ;; Reset my-compile-dir-list
  (setq my-compile-dir-list default-compile-dirs)

  ;; Add pika-dir unless the standard catch will get it
  (unless (string-match "/[a-z-]*monza" pika-dir)
    (let (dir)
      (dolist (subdir pika-subdirs)
	(setq dir (file-truename (concat pika-dir "/" subdir "/")))
	(pika-compile-dir-append (concat "^" dir))))
    (pika-compile-dir-append (format "^%s/" pika-dir)))

  ;; Try to catch the 99% case
  (dolist (subdir pika-subdirs)
    (pika-compile-dir-append (concat "^.*/[a-z-]*monza/software/" subdir "/")))

  (dolist (subdir pika-subdirs)
    (pika-compile-dir-append (concat "^.*/[a-z-]*monza/" subdir "/")))

  (dolist (subdir '("testing/artstests" "testing/arts"))
    (pika-compile-dir-append (concat "^.*/[a-z-]*monza/" subdir "/")))

  ;; Special case for git ARTS directory
  (dolist (subdir '("testing/artstests" "testing/arts"))
    (pika-compile-dir-append (concat "^.*/ARTS/" subdir "/")))

  (pika-compile-dir-append "^.*/[a-z-]*monza/")

  ;; SAM Hack for now
  (let ((dir (file-truename "~work/open_warp/libpri/trunk/dahdi/linux")))
    (when (file-directory-p dir)
      (setq my-compile-dir-list
	    (append my-compile-dir-list
		    (list (list (concat "^" dir "/")))))))

  (unless noninteractive
    (message "PIKA_DIR %s" pika-dir)))

;; Now default it
(if pika-dir
    (set-pika-dir pika-dir)
  (message "\nWARNING: pika-dir not set.\n"))

(defun warp-use-stock-kernel ()
  (interactive)
  (setenv "KERNELSRCDIR" "~work/taco/linux-warped"))

;; -------------------------------------------------------------------

(setq pika-cflags "-DPIKA_DEVEL -Wall")

(when (would-like 'ppc-env)
  (setq ppc-kernel-dir (expand-file-name "~work/taco/linux-2.6/"))
  (setq ppc-u-boot-dir (expand-file-name "~work/taco/u-boot/")))

(defun pika-linux (dir &optional arg)
  (c-set-style "linux")
  (setq lxr-url "http://alice.pikatech.com/lxr/http/"
	lxr-base dir
	lxr-version nil
	lxr-arch nil
	))

;; Add some other kernels
(dolist (kernel '("~work/linux[^/]+" "~work/taco/linux[^/]+"))
  (setq my-compile-dir-list
	(add-to-list 'my-compile-dir-list
		     (list (expand-file-name kernel) nil 'pika-linux)
		     t)))

;; -------------------------------------------------------------------

(when (would-like 'hide-copyleft)
  (add-to-list
   'copylefts-to-hide
   '("^.. IF YOU DO NOT AGREE WITH THE FOLLOWING" .
     "\\(----------\\|===========\\)")))

;; -------------------------------------------------------------------

(my-feature-cond
 (xemacs
  (define-key read-file-name-map [f5]
    '(lambda () (interactive) (insert pika-dir "/")))))

(defun pika-setup-smerge ()
  (interactive)
  (require 'smerge)
  (loop for ignore in '(".cvsignore" ".depend" "TAGS" "*.so" "*.rc"
			"pksystemver.h" "*.mod.c" "*.so.2*"
			"Module.symvers")
    do
    (add-to-list 'smerge-diff-excludes ignore t))
  (delete "*.cmd" smerge-diff-excludes)
  (add-to-list 'smerge-diff-excludes "*.o.cmd")
  (add-to-list 'smerge-diff-excludes "*.ko.cmd")
  ;; We need to make sure hspapps.lib stays up to date!
  (delete "*.lib" smerge-diff-excludes))

(defun smerge-pika (svn-dir git-dir &optional subdir)
  (setq svn-dir (expand-file-name svn-dir))
  (setq git-dir (expand-file-name git-dir))

  (unless (file-directory-p svn-dir)
    (error "%s not a directory" svn-dir))
  (unless (file-directory-p git-dir)
    (error "%s not a directory" git-dir))

  (when subdir
    (let (test-dir)
      (setq test-dir (expand-file-name (concat svn-dir "/" subdir)))
      (when (file-directory-p test-dir)
	(setq svn-dir test-dir))
      (setq test-dir (expand-file-name (concat git-dir "/" subdir)))
      (when (file-directory-p test-dir)
	(setq git-dir test-dir))))

  (pika-setup-smerge)

  (smerge nil svn-dir git-dir))

(defun smerge-monza ()
  (interactive)
  (smerge-pika "~work/svn-monza" "~work/git-monza" "software"))

(defun smerge-arts ()
  (interactive)
  (smerge-pika "~work/svn-monza/testing" "~work/ARTS/testing"))

;; For backwards compatibility with pre 2.9
(let ((ipp "/opt/intel/ipp/5.1/ia32"))
  (when (file-exists-p ipp)
    (setenv "IPP" ipp)))

;; Grab the latest version....
(let ((ipp-list (directory-files "/opt/intel/ipp" t "6\.1.*" nil 'dir)))
  (dolist (ipp ipp-list) (setenv "IPP61" ipp)))

(setq signed-off-by-sig (concat user-full-name " <smaclennan@pikatech.com>"))

;; For grandprix
(setenv "AOH_LIB" (concat pika-dir "/user/libs"))
(setenv "AOH_INC" (concat pika-dir "/include"))

;; For chan_pika
;(setenv "AOH_INC" (concat pika-dir "/sdk/include"))
