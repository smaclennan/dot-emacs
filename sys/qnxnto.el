;;;###autoload
(defun sys-nproc ()
  "Return the number of processors reported by pidin."
  (with-temp-buffer
    (call-process "pidin" nil t nil "info")
    (setq sys-nproc (count-matches "^Processor" (point-min)))))

;;;###autoload
(defun sys-mem ()
  "Return the total and free memory."
  (unless sys-mem
    ;; First time we need to call pidin to get total mem
    (with-temp-buffer
      (call-process "pidin" nil t nil "info")
      (re-search-backward "Freemem:[0-9]+MB/\\([0-9]+\\)MB")
      (setq sys-mem (* (string-to-number (match-string 1)) #x100000))))
  (list sys-mem
	;; Yes ls -ld /proc = free memory
	(nth 7 (file-attributes "/proc"))))

(load "cpuid" nil noninteractive)

;;;###autoload
(defun sys-cpuinfo () (cpuid-cpuinfo))

;;;###autoload
(defun sys-cpu-flags () (cpuid-cpu-flags))

;;;###autoload
(defun sys-is-guest () (cpuid-is-guest))
