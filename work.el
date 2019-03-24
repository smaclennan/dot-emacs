(defun qnx-load-qnx ()
  (interactive)
  (let ((qnx-base (file-name-as-directory (getenv "QNX_BASE_DIR"))))
    (and qnx-base (getenv "QNX_SANDBOX")
	 (load (concat qnx-base "emacs/qnx") t))))

(condition-case nil (qnx-load-qnx) (error nil))
