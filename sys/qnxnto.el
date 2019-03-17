
(defun sys-nproc ()
  "Return the number of processors reported by pidin."
  (with-temp-buffer
    (call-process "pidin" nil t nil "info")
    (count-matches "^Processor" (point-min))))
