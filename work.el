(let ((qnx-base (getenv "QNX_BASE_DIR"))
      (qnx-sandbox (getenv "QNX_SANDBOX")))
  (and qnx-base qnx-sandbox
       (setq qnx-base (file-name-as-directory qnx-base))
       (setq qnx-sandbox (file-name-as-directory qnx-sandbox))
       (load (concat qnx-base "emacs/qnx") t)))
