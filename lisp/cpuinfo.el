;;; cpuinfo.el --- simple interface to /proc/cpuinfo
;; Copyright (C) 2010-2019 Sean MacLennan

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;;###autoload
(defun cpuinfo-num-processors (&optional show)
  "Return number of processors.
This is the most generic of the cpuinfo-* functions. Should work
on Linux, BSD, QNX, and Windows."
  (interactive "p")
  (let ((procs (if (fboundp 'sys-nproc) (sys-nproc) 1)))
    (when show (message "Procs: %d" procs))
    procs))

;;;###autoload
(defun cpuinfo (&optional show)
  "Returns a list describing the type of CPU(s) installed, X86
centric. The list returned is '(vendor family model step). The
`vendor' is a string, all others are numbers."
  (interactive "p")
  (let* ((info
	  (if (fboundp 'sys-cpuinfo)
	      (sys-cpuinfo)
	    (cdr (cpuinfo-cpuid))))
	 (vendor (car info)))

    ;; Pretty print common vendor ids
    (cond
     ((string= "GenuineIntel" vendor) (setcar info "Intel"))
     ((string-match "Authentic ?AMD" vendor) (setcar info "AMD"))
     ((string= "CentaurHauls" vendor) (setcar info "VIA")))

    (when show (message "Vendor %s Family %d Model %d Step %d"
			(nth 0 info) (nth 1 info) (nth 2 info) (nth 3 info)))
    info))

;;;###autoload
(defun cpuinfo-name (&optional show)
  "Returns the model name."
  (interactive "p")
  (let ((name
	 (if (fboundp 'sys-model-name)
	     (sys-model-name)
	   (car (cpuinfo-cpuid)))))
    (when show (message "%s" name))
    name))

(defun cpuinfo-cpuid ()
  (let ((exe (executable-find "cpuid")))
    (unless exe
      (if (string-match "x86" (uname "-m"))
	  (error (concat "Not supported. Maybe install "
			 user-emacs-directory "src/cpuid."))
	(error "Not supported")))
    (with-temp-buffer
      (shell-command exe t)
      (list
       (cpuinfo-find "Model Name")
       (cpuinfo-find "Vendor")
       (string-to-number (cpuinfo-find "Family"))
       (string-to-number (cpuinfo-find "Model"))
       (string-to-number (cpuinfo-find "Stepping"))))))

(defun cpuinfo-get ()
  "Read /proc/cpuinfo into a buffer.
If the buffer already exists, do nothing."
  (unless (eq system-type 'gnu/linux) (error "Not supported."))
  (find-file-noselect "/proc/cpuinfo"))

(defun cpuinfo-find (field)
  "Find a field in cpuinfo output."
  (goto-char (point-min))
  (re-search-forward (concat "^" field "[ \t]+: \\(.*\\)$"))
  (match-string 1))

;;;###autoload
(defun cpuinfo-num-cores (&optional show)
  "Count physical processor cores. Returns a list with three elements:
the number of cores, the number of processors, and a hyperthreaded
flag."
  (interactive "p")
  (with-current-buffer (cpuinfo-get)
    (let (phy cores (procs 0) hyper total)
      ;; Count the physical processors
      (goto-char (point-min))
      (while (re-search-forward "^physical id[ \t]+: \\([0-9]+\\)" nil t)
	(add-to-list 'phy (string-to-number (match-string 1))))
      (if phy
	  (setq phy (length phy))
	(setq phy 1))

      ;; Because Linux only allows SMP, we can just use the first
      ;; cpu cores entry.
      (goto-char (point-min))
      (if (re-search-forward "^cpu cores[ \t]+: \\([0-9]+\\)" nil t)
	  (setq cores (string-to-number (match-string 1)))
	(setq cores 1))

      ;; Count the possibly virtual processors
      (goto-char (point-min))
      (while (re-search-forward "^processor" nil t)
	(setq procs (1+ procs)))

      (setq total (* cores phy))
      (when (not (eq procs total))
	(if (eq procs (* total 2))
	    (setq hyper t)
	  (error "Bad number procs %d phy %d cores %d" procs phy cores)))

      (when show (message "Cores: %d Procs: %d%s = %d" cores phy
			  (if hyper " hyperthreaded" "")
			  (if hyper (* cores phy 2) (* cores phy))))
      (list cores phy hyper))))


;;;###autoload
(defun cpuinfo-flags (&optional show)
  "Returns the cpu flags as a sorted list. Sorting the list makes it easier to find individual flags."
  (interactive "p")
  (let (flags)
    (with-current-buffer (cpuinfo-get)
      (setq flags (sort (split-string (cpuinfo-find "flags")) 'string<)))
    (when show (message "%S" flags))
    flags))

(defun cpuinfo-has-flag (flag)
  "Does the cpu have FLAG defined."
  (car (member flag (cpuinfo-flags))))

;;;###autoload
(defun cpuinfo-show-flag (flag)
  "Does the cpu have FLAG defined."
  (interactive "sFlag: ")
  (message "%S" (cpuinfo-has-flag flag)))

(provide 'cpuinfo)
