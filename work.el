(defun qnx-load-qnx ()
  (interactive)
  (when (getenv "QNX_SANDBOX")
    (let ((base (getenv "QNX_BASE_DIR")))
      (unless base (setq base "~/work"))
      (load (concat (file-name-as-directory base) "emacs/qnx")))))

(qnx-load-qnx)
