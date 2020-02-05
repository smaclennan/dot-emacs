;; Common variables and functions for Unix(ish) systems.

(eval-and-compile (require 'sam-common)) ;; for uname

(defvar sys-nproc nil "Total number of processors.")

(defvar sys-mem nil "Total system memory.")

(defvar sys-arch
  (let ((arch (uname "-m")))
    (cond
     ((string-match "x86\\|amd64" arch) 'x86)
     ((string-match "arm\\|aarch" arch) 'arm)
     (t 'unknown)))
  "Lisp friendly version of arch.")

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

;; Linux and NetBSD
(defun sys-meminfo ()
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

;;;###autoload
(defun sysctl (arg)
  "Return sysctl ARG as a number."
  (string-to-number (shell-command-to-string (concat "sysctl -n " arg))))

;;;###autoload
(defun sysctl-str (arg)
  "Return sysctl ARG as a string."
  (shell-command-to-string (concat "sysctl -n " arg)))

;;;###autoload
(defun sys-is-guest ()
  (member "hypervisor" (sys-cpu-flags)))

;; ----------------------------------------------------------------------
;; Linux overrides all the rest

;;;###autoload
(defun sys-os () (list (uname "-s") (uname "-r")))

;;;###autoload
(defun sys-cpuinfo () (cpuid-cpuinfo))

;;;###autoload
(defun sys-cpu-flags () (cpuid-cpu-flags))
