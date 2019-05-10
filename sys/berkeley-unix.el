(defvar page-size nil "Page size filled in by `sys-mem'.")

(defun sysctl (arg)
  "Return sysctl ARG as a number"
  (string-to-number (shell-command-to-string (concat "sysctl -n " arg))))

;;;###autoload
(defun sys-nproc ()
  "Return number of cpus."
  (sysctl "hw.ncpu"))

;;;###autoload
(defun sys-mem ()
  "Return total and free memory."
  (unless page-size (setq page-size (sysctl "hw.pagesize")))
  (list (sysctl "hw.realmem")
	(* (sysctl "vm.stats.vm.v_free_count") page-size)))
