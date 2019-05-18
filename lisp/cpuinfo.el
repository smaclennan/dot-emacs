;;; cpuinfo.el --- simple interface to /proc/cpuinfo
;; Copyright (C) 2010-2019 Sean MacLennan

;; This program is free software; you can redistribute it and/or modify

(eval-when-compile (require 'sam-common)) ;; for uname
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

(defun cpuinfo-cpuid-exe ()
  (let ((exe (executable-find "cpuid")))
    (unless exe
      (if (string-match "x86" (uname "-m"))
	  (error (concat "Not supported. Maybe install "
			 user-emacs-directory "src/cpuid."))
	(error "Not supported")))
    exe))

(defun cpuinfo-cpuid ()
  (let ((exe (cpuinfo-cpuid-exe)))
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
(defun cpuinfo-flags (&optional show)
  "Returns the cpu flags as a sorted list. Sorting the list makes it easier to find individual flags."
  (interactive "p")
  (let (flags)
    (if (eq system-type 'gnu/linux)
	(with-current-buffer (cpuinfo-get)
	  (setq flags (cpuinfo-find "flags")))
      (setq flags (cpuinfo-cpuid-flags)))
    (setq flags (sort (split-string flags) 'string<))
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

(defun cpuinfo-diff-flags ()
  "Only makes sense on Linux"
  (interactive)
  (let (flags-full flags missing)
    (with-current-buffer (cpuinfo-get)
      (setq flags-full (split-string (cpuinfo-find "flags"))))
    (setq flags (split-string (cpuinfo-cpuid-flags)))
    (dolist (flag flags-full)
      (unless (member flag flags)
	(add-to-list 'missing flag)))
    (message "Missing %d: %S" (length missing) missing)
    ))

(provide 'cpuinfo)
