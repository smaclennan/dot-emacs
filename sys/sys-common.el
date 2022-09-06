;; Functions that rely on the sys functions

;;;###autoload
(defun cpuinfo ()
  "Describes the current cpus."
  (interactive)
  (let ((info (sys-cpuinfo)))
    (message "%s Vendor %s Family %d Model %d Step %d Procs %d"
	     (nth 0 info) (nth 1 info) (nth 2 info) (nth 3 info)
	     (nth 4 info) (sys-nproc))))

;;;###autoload
(defun cpuinfo-has-flag (flag &optional show)
  "Does the cpu have FLAG defined."
  (interactive "sFlag: \np")
  (let ((has-flag (member flag (sys-cpu-flags))))
    (when show
      (message "%s %s" flag (if has-flag "yes" "no")))
    has-flag))

(defun mem-human-readable (mem)
  "Convert MEM in bytes to human readable form."
  (cond
   ((> mem 1073741824)
    (format "%.1fG" (/ mem 1073741824.0)))
   ((> mem 1048576)
    (format "%.1fM" (/ mem 1048576.0)))
   (t
    (format "%.1fK" (/ mem 1024.0)))))

;;;###autoload
(defun memory ()
  "Display total and free memory."
  (interactive)
  (let ((mem (sys-mem)))
    (message "%s"
	     (concat "total " (mem-human-readable (car mem))
		     "  free " (mem-human-readable (cadr mem))
		     (when (nth 2 mem)
		       (concat "  avail " (mem-human-readable (nth 2 mem))))))))

;;;###autoload
(defun sys-info ()
  "System info in human readable form."
  (interactive)
  (with-output-to-temp-buffer "*sys-info*"
    (let ((os (sys-os)) (mem (sys-mem))
	  (distro (if (fboundp 'sys-linux-distro) (sys-linux-distro))))
      (princ (concat
	      (if (sys-is-guest) "Guest ") (car os) " " (cadr os) "\n"
	      (if distro (concat (car distro) " " (cadr distro)  "\n"))
	      (car (sys-cpuinfo)) " (" (number-to-string (sys-nproc)) ")\n"
	      "Memory: " (mem-human-readable (car mem))
	      "  free " (mem-human-readable (cadr mem))
	      (when (nth 2 mem)
		(concat "  avail " (mem-human-readable (nth 2 mem))))
	      "\n")))))
