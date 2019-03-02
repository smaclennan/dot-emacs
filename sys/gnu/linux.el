(defun sys-nproc ()
  (if (file-exists-p "/sys/bus/cpu/devices")
      (length (directory-files "/sys/bus/cpu/devices" nil "cpu[0-9]*"))
    (with-temp-buffer
      (call-process "cat" nil t nil "/proc/cpuinfo")
      (count-matches "^processor" (point-min) (point-max)))))
