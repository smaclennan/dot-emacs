(setq compile-command "gmake ")
(setq make-clean-command "gmake clean all")

(setq local-compile-cc "cc -O2 -Wall")
(setq local-compile-c++ "c++ -O2 -Wall")

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
  (unless sys-mem (setq sys-mem (sysctl "hw.realmem")))
  (list sys-mem
	(* (sysctl "vm.stats.vm.v_free_count") page-size)))

(load "cpuid")

;;;###autoload
(defun sys-cpuinfo () (cpuid-cpuinfo))

;;;###autoload
(defun sys-cpu-flags () (cpuid-cpu-flags))

;;;###autoload
(defun sys-is-guest () (cpuid-is-guest))
