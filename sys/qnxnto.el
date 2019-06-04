;;;###autoload
(defun sys-nproc ()
  "Return the number of processors reported by pidin."
  (with-temp-buffer
    (call-process "pidin" nil t nil "info")
    (count-matches "^Processor" (point-min))))

(defvar sys-total-mem nil
  "Total system memory. Filled in by `sys-mem'.")

;;;###autoload
(defun sys-mem ()
  "Return the total and free memory."
  (unless sys-total-mem
    ;; First time we need to call pidin to get total mem
    (with-temp-buffer
      (call-process "pidin" nil t nil "info")
      (re-search-backward "Freemem:[0-9]+MB/\\([0-9]+\\)MB")
      (setq sys-total-mem (* (string-to-number (match-string 1)) #x100000))))
  (list sys-total-mem
	;; Yes ls -ld /proc = free memory
	(nth 7 (file-attributes "/proc"))))
