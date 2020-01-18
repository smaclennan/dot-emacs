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

;;;###autoload
(defun sysctl (arg)
  "Return sysctl ARG as a number."
  (string-to-number (shell-command-to-string (concat "sysctl -n " arg))))


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
