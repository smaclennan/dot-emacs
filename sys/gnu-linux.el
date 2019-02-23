
(defvar sys-cpu-dir "/sys/bus/cpu/devices"
  "The /sys directory containing cpu* files.")

;; Used by `cpuinfo-num-processors'
(defun sys-nproc ()
  (if (file-exists-p sys-cpu-dir)
      ;; Does this even not exist any more?
      (length (directory-files sys-cpu-dir nil "cpu[0-9]*"))
    (let ((procs))
      (with-temp-buffer
	;; insert-file-contents does not work on /proc
	(call-process "cat" nil t nil "/proc/cpuinfo")
	(goto-char (point-min))
	(while (re-search-forward "^processor" nil t)
	  (setq procs (1+ procs))))
      procs)))
