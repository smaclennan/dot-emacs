(defun qnx-load-qnx ()
  (interactive)
  (let ((base (getenv "QNX_BASE_DIR")))
    (and base (getenv "QNX_SANDBOX")
	 (load (concat (file-name-as-directory base) "emacs/qnx") t))))

(qnx-load-qnx)
