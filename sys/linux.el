;;;###autoload
(defun sys-nproc ()
  "Return number of cpu devices."
  (if (file-exists-p "/sys/bus/cpu/devices")
      (length (directory-files "/sys/bus/cpu/devices" nil "cpu[0-9]*"))
    (with-current-buffer (find-file-noselect "/proc/cpuinfo")
      (count-matches "^processor" (point-min)))))

(defun cpuinfo-find (field)
  "Find a field in cpuinfo output."
  (goto-char (point-min))
  (re-search-forward (concat "^" field "[ \t]+: \\(.*\\)$"))
  (match-string 1))

;;;###autoload
(defun sys-cpuinfo ()
  (with-current-buffer (find-file-noselect "/proc/cpuinfo")
    (list (cpuinfo-find "vendor_id")
	  (string-to-number (cpuinfo-find "cpu family"))
	  (string-to-number (cpuinfo-find "model"))
	  (string-to-number (cpuinfo-find "stepping")))))

;;;###autoload
(defun sys-model-name ()
  (with-current-buffer (find-file-noselect "/proc/cpuinfo")
    (cpuinfo-find "model name")))

;;;###autoload
(defun sys-mem ()
  "Report total and free memory."
  (let (total free)
    (with-temp-buffer
      (insert-file-contents "/proc/meminfo")
      (goto-char (point-min))
      (re-search-forward "^Memtotal: *\\([0-9]+\\) kB$")
      (setq total (* (string-to-number (match-string 1)) 1024))
      (re-search-forward "^Memavailable: *\\([0-9]+\\) kB$")
      (setq free (* (string-to-number (match-string 1)) 1024)))
    (list total free)))
