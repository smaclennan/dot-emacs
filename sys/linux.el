(defvar sys-nproc nil "Total number of processors.")

(defvar sys-mem nil "Total system memory.")

;;;###autoload
(defun sys-nproc ()
  "Return number of cpu devices."
  (condition-case nil
      (length (directory-files "/sys/bus/cpu/devices" nil "cpu[0-9]*" t))
    (error
     (with-temp-buffer
       (insert-file-contents "/proc/cpuinfo")
       (count-matches "^processor" (point-min) (point-max))))))

;;;###autoload
(defun sys-mem ()
  "Report total, free, and available memory."
  (with-temp-buffer
    (insert-file-contents "/proc/meminfo")
    (goto-char (point-min))
    (re-search-forward "^Memtotal: *\\([0-9]+\\) kB$")
    (setq sys-mem (* (string-to-number (match-string 1)) 1024))
    (re-search-forward "^Memfree: *\\([0-9]+\\) kB$")
    (list sys-mem (* (string-to-number (match-string 1)) 1024)
	  ;; Older kernels do not have Memavailable
	  (when (re-search-forward "^Memavailable: *\\([0-9]+\\) kB$" nil t)
	    (* (string-to-number (match-string 1)) 1024)))))

(defun cpuinfo-find (field)
  "Find a field in cpuinfo output."
  (goto-char (point-min))
  (re-search-forward (concat "^" field "[ \t]+: \\(.*\\)$"))
  (match-string 1))

;;;###autoload
(defun sys-cpuinfo ()
  (with-current-buffer (find-file-noselect "/proc/cpuinfo")
    (list (cpuinfo-find "model name")
	  (cpuinfo-find "vendor_id")
	  (string-to-number (cpuinfo-find "cpu family"))
	  (string-to-number (cpuinfo-find "model"))
	  (string-to-number (cpuinfo-find "stepping")))))

;;;###autoload
(defun sys-cpu-flags ()
  (with-current-buffer (find-file-noselect "/proc/cpuinfo")
    (split-string (cpuinfo-find "flags"))))

;;;###autoload
(defun sys-is-guest ()
  (with-current-buffer (find-file-noselect "/proc/cpuinfo")
    (string-match "\\bhypervisor\\b" (cpuinfo-find "flags"))))
