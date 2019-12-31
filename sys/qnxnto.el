(defvar sys-nproc nil "Total number of processors.")

(defvar sys-mem nil "Total system memory.")

(defun do-pidin-info ()
  (with-temp-buffer
    (call-process "pidin" nil t nil "info")
    (re-search-backward "Freemem:[0-9]+MB/\\([0-9]+\\)MB")
    (setq sys-mem (* (string-to-number (match-string 1)) #x100000))
    (setq sys-nproc (count-matches "^Processor"))))

;;;###autoload
(defun sys-os ()
  (list "QNX" (uname "-r")))

;;;###autoload
(defun sys-nproc ()
  "Return the number of processors reported by pidin."
  (if sys-nproc sys-nproc (do-pidin-info)))

;;;###autoload
(defun sys-mem ()
  "Return the total and free memory."
  (unless sys-mem (do-pidin-info))
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
