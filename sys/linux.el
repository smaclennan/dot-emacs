(defvar sys-nproc nil "Total number of processors.")

(defvar sys-mem nil "Total system memory.")

(defvar sys-os nil "Filled in by `sys-os'.")

;;;###autoload
(defun sys-os ()
  (if sys-os
      sys-os
    (if (file-exists-p "/etc/os-release")
	(with-temp-buffer
	  (insert-file-contents "/etc/os-release")
	  (re-search-forward "^NAME=\"?\\([^\"]+\\)\"?$")
	  (let ((distro (match-string 1)))
	    (if (string-match "^Red Hat" distro)
		(setq distro "Red Hat")
	      (setq distro (car (split-string distro))))
	    (re-search-forward "^VERSION=\"\\([^\"]+\\)\"$")
	    (setq sys-os (list distro (match-string 1)))))
      '("Linux" "unknown"))))

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
  (re-search-forward (concat "^" field "[ \t]*: \\(.*\\)$"))
  (match-string 1))

(defun arch-strings ()
  (let ((arch (uname "-m")))
    (cond
     ((string-match "x86" arch)
      '("flags" "model name" "vendor_id" "cpu family" "model" "stepping"))
     ((string-match "arm\\|aarch" arch)
      '("Features" "Processor" "CPU Implementer" "CPU architecture" "CPU variant"
	"CPU part"))
     (t (error "Arch %s not supported" arch)))))

;;;###autoload
(defun sys-cpuinfo ()
  (let ((strs (arch-strings)))
    (with-current-buffer (find-file-noselect "/proc/cpuinfo")
      (list (cpuinfo-find (nth 1 strs))
	    (cpuinfo-find (nth 2 strs))
	    (strtol (cpuinfo-find (nth 3 strs)))
	    (strtol (cpuinfo-find (nth 4 strs)))
	    (strtol (cpuinfo-find (nth 5 strs)))))))

;;;###autoload
(defun sys-cpu-flags ()
  (let ((strs (arch-strings)))
    (with-current-buffer (find-file-noselect "/proc/cpuinfo")
      (split-string (cpuinfo-find (car strs))))))

;;;###autoload
(defun sys-is-guest ()
  (with-current-buffer (find-file-noselect "/proc/cpuinfo")
    (string-match "\\bhypervisor\\b" (cpuinfo-find "flags"))))
