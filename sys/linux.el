(defvar sys-nproc nil "Total number of processors.")

(defvar sys-mem nil "Total system memory.")

(defvar sys-os nil "Filled in by `sys-os'.")

(require 'sam-common)

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

(defvar sys-arch
  (let ((arch (uname "-m")))
    (cond
     ((string-match "x86" arch) 'x86)
     ((string-match "arm\\|aarch" arch) 'arm)
     (t 'unknown)))
  "Lisp friendly version of arch")

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

(defconst arm-implementer
  '((#x41 "ARM")     (#x42 "Broadcom") (#x43 "Cavium")   (#x44 "DEC")
    (#x4e "Nvidia")  (#x50 "APM")      (#x51 "Qualcomm") (#x53 "Samsung")
    (#x56 "Marvell") (#x69 "Intel")))

(defun sys-vendor (str)
  (if (eq sys-arch 'x86)
      (cond
       ((string= "GenuineIntel" str) "Intel")
       ((string-match "Authentic ?AMD" str) "AMD")
       ((string= "CentaurHauls" str) "VIA")
       (t str))
    (if (eq sys-arch 'arm)
	(let ((vendor (assoc (strtol str) arm-implementer)))
	  (if vendor (cadr vendor) str))
      str)))

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

;;;###autoload
(defun sys-is-guest ()
  (member "hypervisor" (sys-cpu-flags)))
