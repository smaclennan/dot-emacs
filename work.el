(let ((qnx-base (getenv "QNX_BASE_DIR")))
  (when qnx-base
    (load (concat (file-name-as-directory qnx-base) "emacs/qnx") t)))
