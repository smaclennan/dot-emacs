(defvar pika-dir (or (getenv "PIKA_DIR") "~/monza/software")
  "* Root of the PIKA source tree.
Use `set-pika-dir' to change this.")

(defvar pika-kernel (getenv "PIKA_KERNEL")
  "* Set this to compile for a different kernel version.
Nil defaults to the currently running kernel.")

(defvar pika-cflags (getenv "PIKA_CFLAGS")
  "* Set any extra kernel flags you want to pass to user mode.")

(defvar pika-kernel-cflags (or (getenv "PIKA_KERNEL_CFLAGS") "-g")
  "* Set any extra kernel flags you want to pass to all the kernel drivers.")

(defvar pika-subdirs '("user"
		       "kernel/hsp" "kernel/primenet" "kernel"
		       "applications/monzatest" "applications"
		       "artstests"
		       )
  "* Pika sub-directories to handle specially.")

;; -------------------------------------------------------------------

(setq pika-cflags "-DUSE_POOL=0")

;; -------------------------------------------------------------------
;; Some evil troll decided that code should be unreadable and declared
;; tabs to be 2 characters. To make matters worse, they decided they
;; must be spaces so we cannot adjust ourselves.....

(c-add-style "pika" '("linux" (c-basic-offset . 2)))

(defun pika-c-mode (dir arg)
  (when pika-kernel
    (setq compile-command
	  (format "%s KERNEL=%s" compile-command pika-kernel)))
  (c-set-style "pika")
  (setq mode-name (concat "PIKA-" mode-name)
	indent-tabs-mode nil
	tab-width 2)
  (force-mode-line-update))

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

  ;; Reset my-compile-dir-list
  (setq my-compile-dir-list my-compile-dir-linux)

  ;; Add pika-dir unless the standard catch will get it
  (unless (string= (file-name-nondirectory pika-dir) "monza")
    (loop for subdir in pika-subdirs do
      (setq my-compile-dir-list
	    (append my-compile-dir-list
		    (list (list (format "^%s/%s/" pika-dir subdir) nil 'pika-c-mode)))))
    (setq my-compile-dir-list
	  (append my-compile-dir-list
		  (list (list (format "^%s/" pika-dir) nil 'pika-c-mode)))))

  ;; Try to catch the 99% case
  (loop for subdir in pika-subdirs do
    (setq my-compile-dir-list
	  (append my-compile-dir-list
		  (list (list (format "^.*/monza/%s/" subdir) nil 'pika-c-mode)))))
    (setq my-compile-dir-list
	  (append my-compile-dir-list
		  '(("^.*/monza/" nil pika-c-mode))))
  )

;; Now default it
(set-pika-dir pika-dir)

;; -------------------------------------------------------------------

(when (would-like 'hide-copyleft)
  (add-to-list
   'copylefts-to-hide
   '(" \\* IF YOU DO NOT AGREE WITH THE FOLLOWING" .
     " \\* -----------------------------------------------------------------")))

;; -------------------------------------------------------------------

(define-key read-file-name-map [f5]
  '(lambda () (interactive) (insert pika-dir "/")))
(define-key read-file-name-map [f6]
  '(lambda () (interactive) (insert pika-dir "/user/")))
(define-key read-file-name-map [f8]
  '(lambda () (interactive) (insert pika-dir "/kernel/")))
