(condition-case nil
    (and
     (setq qnx-base (file-name-as-directory (getenv "QNX_BASE_DIR")))
     (setq qnx-sandbox (file-name-as-directory (getenv "QNX_SANDBOX")))
     (load (concat qnx-base "emacs/qnx") t))
  (error nil))
