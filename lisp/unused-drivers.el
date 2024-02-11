;; -*- lexical-binding: t -*-
;; Very Linux specific
;; Hint to self: Self, you cannot scp /proc/modules.

(defvar module-whitelist '("usb_storage" "uas" "mmc_block"
			   "psnap" "p8022" ;; bridge
			   "psmouse"
			   "isofs" "qnx6"
			   )
  "*Vetted modules to whitelist.
If this is the first run, you might want to nil this out.
NOTE: They must have underscores (_) not dashes (-)!")

(defvar lsmod-file nil
  "*File containing list of modules.
This is mainly for vetting embedded computers. If nil, defaults
to /proc/modules.")

(defun build-drivers-list (release)
  (with-temp-buffer
    (let ((moddep (concat "/lib/modules/" release "/modules.dep"))
	  dlist lines)
      (insert-file-contents moddep nil nil nil t)
      (setq lines (count-lines (point-min) (point-max)))
      (goto-char (point-min))
      (while (re-search-forward ".*/\\([A-Za-z0-9_-]+\\)\.ko:.*$" nil t)
	;; Get the basename and convert - to _
	(setq dlist (cons (replace-regexp-in-string "-" "_" (match-string 1) nil t) dlist)))
      ;; Sanity check
      (unless (eq lines (length dlist)) (error "DRIVERS MISMATCH: lines %d list %d" lines (length dlist)))
    dlist)))

(defun build-lsmod-list ()
  (let ((file (if lsmod-file lsmod-file "/proc/modules")))
    (with-temp-buffer
      (let (llist lines)
	(insert-file-contents file nil nil nil t)
	(goto-char (point-min))
	(when (looking-at "Module") (kill-whole-line))
	(setq lines (count-lines (point-min) (point-max)))
	(while (re-search-forward "^\\([^ ]+\\) .*" nil t)
	  (setq llist (cons (match-string 1) llist)))
	;; Sanity check
	(unless (eq lines (length llist)) (error "LSMOD MISMATCH: lines %d list %d" lines (length llist)))
	llist))))

(defun dir-mods (release dir)
  (setq dir (concat "/lib/modules/" release "/kernel/" dir))
  (mapcar (lambda (file) (file-name-sans-extension file))
	  (directory-files dir nil ".*\\.ko")))

;;;###autoload
(defun unused-drivers (release)
  "List all the drivers that are installed, but not used.
Prompts for the kernel release to test against.

You can set `module-whitelist' for a list of modules you want,
but may not have been installed. For example dynamic USB
devices."
  (interactive (list
		(let ((rel (uname "-r")))
		  (read-string (format "Release [%s]: " rel) nil nil rel))))
  (let ((used-list (build-lsmod-list))
	(driver-list (build-drivers-list release))
	(kdir (concat "/lib/modules/" release "/kernel/"))
	dcount ucount fcount)
    (setq dcount (length driver-list) ucount (length used-list))
    (dolist (mod used-list)
      (setq driver-list (delete mod driver-list)))
    (setq fcount (length driver-list))
    ;; Sanity check
    (unless (eq dcount (+ ucount fcount))
      (error "dcount %d != ucount %d + fcount %d" dcount ucount fcount))

    ;; Remove the whitelisted modules - do this after counts
    (dolist (mod module-whitelist)
      (setq driver-list (delete mod driver-list)))

    (when module-whitelist
      ;; Remove usb serial drivers
      (dolist (mod (dir-mods release "drivers/usb/serial"))
	(setq driver-list (delete mod driver-list)))

      ;; Remove iptables
      (dolist (mod (dir-mods release "net/netfilter"))
	(setq driver-list (delete mod driver-list)))
      (dolist (mod (dir-mods release "net/ipv4/netfilter"))
	(setq driver-list (delete mod driver-list)))
      )

    (with-current-buffer (get-buffer-create "*driver list*")
      (erase-buffer)
      (dolist (mod driver-list)
	(insert (concat mod "\n")))
      (insert (format "\n%d unused drivers\n" (length driver-list))))

    (if driver-list
	(display-buffer "*driver list*")
      (message "No unused drivers."))))
