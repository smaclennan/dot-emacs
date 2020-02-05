(load "unix" nil noninteractive)

;;;###autoload
(defun sys-os ()
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
    '("Linux" "unknown")))

;;;###autoload
(defun sys-nproc ()
  "Return number of cpu devices."
  (if sys-nproc
      sys-nproc
    (setq sys-nproc
	  (condition-case nil
	      (length (directory-files "/sys/bus/cpu/devices" nil "cpu[0-9]*" t))
	    (error
	     (with-temp-buffer
	       (insert-file-contents "/proc/cpuinfo")
	       (count-matches "^processor" (point-min) (point-max))))))))

;;;###autoload
(defun sys-mem ()
  "Report total, free, and available memory."
  (sys-meminfo))

(defun cpuinfo-find (field)
  "Find a field in cpuinfo output."
  (goto-char (point-min))
  (re-search-forward (concat "^" field "[ \t]*: \\(.*\\)$"))
  (match-string 1))

(defun arch-strings ()
  (if (eq sys-arch 'x86)
      '("flags" "model name" "vendor_id" "cpu family" "model" "stepping")
    (if (eq sys-arch 'arm)
	'("Features" "Processor" "CPU Implementer" "CPU architecture"
	  "CPU variant" "CPU part")
      (error "Arch %s not supported" arch))))

;;;###autoload
(defun sys-cpuinfo ()
  (let ((strs (arch-strings)))
    (with-temp-buffer
      (insert-file-contents "/proc/cpuinfo")
      (list (cpuinfo-find (nth 1 strs))
	    (sys-vendor (cpuinfo-find (nth 2 strs)))
	    (strtol (cpuinfo-find (nth 3 strs)))
	    (strtol (cpuinfo-find (nth 4 strs)))
	    (strtol (cpuinfo-find (nth 5 strs)))))))

;;;###autoload
(defun sys-cpu-flags ()
  (let ((strs (arch-strings)))
    (with-temp-buffer
      (insert-file-contents "/proc/cpuinfo")
      (split-string (cpuinfo-find (car strs))))))
