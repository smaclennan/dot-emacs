(defun sys-nproc ()
  "Return number of cpu devices."
  (if (file-exists-p "/sys/bus/cpu/devices")
      (length (directory-files "/sys/bus/cpu/devices" nil "cpu[0-9]*"))
    (with-temp-buffer
      (call-process "cat" nil t nil "/proc/cpuinfo")
      (count-matches "^processor" (point-min)))))

(defun sys-mem ()
  "Report total and free memory."
  (let (total free)
    (with-temp-buffer
      (call-process "cat" nil t nil "/proc/meminfo")
      (goto-char (point-min))
      (re-search-forward "^Memtotal: *\\([0-9]+\\) kB$")
      (setq total (* (string-to-number (match-string 1)) 1024)))
      (re-search-forward "^Memfree: *\\([0-9]+\\) kB$")
      (setq free (* (string-to-number (match-string 1)) 1024))
    (list total free)))
