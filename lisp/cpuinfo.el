;;; cpuinfo.el --- CPU info... and memory too
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
(defun cpuinfo ()
  "Describes the current cpus."
  (interactive)
  (let* ((info (sys-cpuinfo))
	 (vendor (cadr info)))
    ;; Pretty print common vendor ids
    (cond
     ((string= "GenuineIntel" vendor) (setq vendor "Intel"))
     ((string-match "Authentic ?AMD" vendor) (setq vendor "AMD"))
     ((string= "CentaurHauls" vendor) (setq vendor "VIA")))

    (message "%s Vendor %s Family %d Model %d Step %d Procs %d"
	     (nth 0 info) vendor (nth 2 info) (nth 3 info)
	     (nth 4 info) (sys-nproc))))

;;;###autoload
(defun cpuinfo-has-flag (flag &optional show)
  "Does the cpu have FLAG defined."
  (interactive "sFlag: \np")
  (let ((has-flag (member flag (cpuinfo-flags))))
    (when show
      (message "%s %s" flag (if has-flag "yes" "no")))
    has-flag))

;; Not really cpuinfo... but makes sense here
(defun mem-human-readable (mem)
  (cond
   ((> mem 1073741824)
    (format "%.1fG" (/ mem 1073741824.0)))
   ((> mem 1048576)
    (format "%.1fM" (/ mem 1048576.0)))
   ((> mem 1024)
    (format "%.1fK" (/ mem 1024.0)))
   (t (format "%d" mem))))

;;;###autoload
(defun memory ()
  "Display total and free memory."
  (interactive)
  (let ((mem (sys-mem)))
    (message "total %s  free %s"
	     (mem-human-readable (car mem))
	     (mem-human-readable (cadr mem)))))

(provide 'cpuinfo)
