;;;###autoload
(defun sys-nproc ()
  "Return the number of processors reported by pidin."
  (with-temp-buffer
    (call-process "pidin" nil t nil "info")
    (count-matches "^Processor" (point-min))))

;;;###autoload
(defun sys-mem ()
  "Return the total and free memory reported by pidin."
  (with-temp-buffer
    (call-process "pidin" nil t nil "info")
    (goto-char (point-min))
    (re-search-forward "Freemem:\\([0-9]+\\)MB/\\([0-9]+\\)MB")
    (list (* (string-to-number (match-string 2)) #x100000)
	  (* (string-to-number (match-string 1)) #x100000))))
