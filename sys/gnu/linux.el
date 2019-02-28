;; Good to Linux 3.4
(defun sys-nproc ()
  (length (directory-files "/sys/bus/cpu/devices" nil "cpu[0-9]*")))
