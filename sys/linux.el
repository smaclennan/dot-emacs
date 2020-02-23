(load "unix" nil noninteractive)

(defun linux-os (file)
  (if (file-exists-p file)
      (with-temp-buffer
	(insert-file-contents file)
	(re-search-forward "^NAME=\"?\\([^\"]+\\)\"?$")
	(let ((distro (car (split-string (match-string 1)))))
	  (if (equal distro "Red") (setq distro "Red Hat"))
	  (re-search-forward "^VERSION=\"\\([^\"]+\\)\"$")
	  (list distro (match-string 1))))
    (list (uname "-s") (uname "-r"))))

;;;###autoload
(defun sys-os () (linux-os "/etc/os-release"))

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
      (error "Arch %S not supported" sys-arch))))

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
