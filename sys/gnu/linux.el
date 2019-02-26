(defun sys-nproc ()
  (length (directory-files "/sys/bus/cpu/devices" nil "cpu[0-9]*")))
