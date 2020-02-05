;; WARNING: This file is untested. This is just some information I
;; gleaned off the net and didn't want to lose.

(load "unix"  nil noninteractive)

;;;###autoload
(defun sys-nproc ()
  (if sys-nproc
      sys-nproc
    (setq sys-nproc (sysctl "machdep.cpu.thread_count"))))

;;;###autoload
(defun sys-cpuinfo ()
  (list (sysctl-str "machdep.cpu.brand_string")
	(sys-vendor (sysctl-str "machdep.cpu.vendor"))
	(sysctl "machdep.cpu.family")
	(sysctl "machdep.cpu.model")
	(sysctl "machdep.cpu.extmodel")))

;;;###autoload
(defun sys-mem ()
  (unless sys-mem
    (setq sys-mem (sysctl "hw.memsize")))
  (list sys-mem
	(with-temp-buffer
	  (call-process "vm_stat" nil t nil)
	  (re-search-backward "^Pages free: *\\([0-9]+\\)")
	  (* (string-to-number (match-string 1)) 4096))))
