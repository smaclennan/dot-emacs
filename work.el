(defvar pika-dir (or (getenv "PIKA_DIR") "~work/monza/software")
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
		       "../testing/artstests" "../testing/arts"
		       )
  "* Pika sub-directories to handle specially.")

(if nil
    ;; ELDK
    (setq ppc-toolchain-dir "/usr/src/eldk")
  ;; PADS
  (setq ppc-toolchain-dir (expand-file-name "~work/pads/toolchain")))

;; -------------------------------------------------------------------

(setq pika-cflags "-DPIKA_DEVEL -Wall")

(when (would-like 'ppc-env)
  (setq ppc-kernel-dir (expand-file-name "~work/taco/linux-2.6/"))
  (setq ppc-u-boot-dir (expand-file-name "~work/taco/u-boot/")))


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
  (c-set-style "pika")
  (setq mode-name (concat "PIKA-" mode-name))
  (setq indent-tabs-mode nil)
  (setq tab-width 2)
  (setq lxr-url "http://alice.pikatech.com/lxr/http"
	lxr-version "monza"
	lxr-base pika-dir)
  (force-mode-line-update))

;; For windows assume we are always editing pika code
(when running-windoze
  (add-hook 'c-mode-common-hook 'pika-c-mode 'append))

;; -------------------------------------------------------------------
(defun set-pika-dir (dir)
  (interactive "Dpika-dir: ")
  (setq pika-dir (expand-file-name dir))
  (when (string-match "/$" pika-dir)
    (setq pika-dir (replace-match "" nil nil pika-dir)))
  (setenv "PIKA_DIR" pika-dir)

  ;; Note: if these are nil, setenv will remove them
  (setenv "PIKA_KERNEL" pika-kernel)
  (setenv "PIKA_CFLAGS" pika-cflags)
  (setenv "PIKA_KERNEL_CFLAGS" pika-kernel-cflags)

  (when (eq (getenv "LD_LIBRARY_PATH") nil)
    (setenv "LD_LIBRARY_PATH" (concat pika-dir "/user/libs")))

  ;; Reset my-compile-dir-list
  (setq my-compile-dir-list my-compile-dir-linux)

  (let (dir)
    ;; Add pika-dir unless the standard catch will get it
    (unless (string-match "/monza/software$" pika-dir)
      (loop for subdir in pika-subdirs do
	(setq dir (file-truename (concat pika-dir "/" subdir "/")))
	(setq my-compile-dir-list
	      (append my-compile-dir-list
		      (list (list (concat "^" dir) nil 'pika-c-mode)))))
      (setq my-compile-dir-list
	    (append my-compile-dir-list
		    (list (list (format "^%s/" pika-dir) nil 'pika-c-mode)))))

    ;; Try to catch the 99% case
    (loop for subdir in pika-subdirs do
      (setq dir (file-truename (concat "/monza/software/" subdir "/")))
      (setq my-compile-dir-list
	    (append my-compile-dir-list
		    (list (list (concat "^.*" dir) nil 'pika-c-mode)))))
    (setq my-compile-dir-list
	  (append my-compile-dir-list
		  '(("^.*/monza/" nil pika-c-mode)))))
  )

;; Now default it
(set-pika-dir pika-dir)

;; -------------------------------------------------------------------

(when (would-like 'hide-copyleft)
  (add-to-list
   'copylefts-to-hide
   '("^.. IF YOU DO NOT AGREE WITH THE FOLLOWING" .
     "\\(----------\\|===========\\)")))

;; -------------------------------------------------------------------

(define-key read-file-name-map [f5]
  '(lambda () (interactive) (insert pika-dir "/")))


(defun pika-setup-smerge ()
  (interactive)
  (require 'smerge)
  (loop for ignore in '(".cvsignore" ".depend" "TAGS" "*.so" "*.rc") do
    (add-to-list 'smerge-diff-excludes ignore t)))

(let ((ipp "/opt/intel/ipp/5.1/ia32"))
  (when (file-exists-p ipp)
    (setenv "IPP" ipp)))

(setq signed-off-by-sig (concat user-full-name " <smaclennan@pikatech.com>"))

;; For grandprix
(setenv "AOH_LIB" (concat pika-dir "/user/libs"))
(setenv "AOH_INC" (concat pika-dir "/include"))

;; For chan_pika
;(setenv "AOH_INC" (concat pika-dir "/sdk/include"))
