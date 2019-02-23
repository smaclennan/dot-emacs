
(defun sys-nproc ()
  "Return the number of processors reported by pidin."
  (let ((nproc 0))
    (with-temp-buffer
      (call-process "pidin" nil t nil "info")
      (goto-char (point-min))
      (while (re-search-forward "^Processor" nil t)
	(setq nproc (1+ nproc))))
    nproc))
