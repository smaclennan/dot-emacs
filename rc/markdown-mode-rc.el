(cond
 ((executable-find "smu")
  ;; https://github.com/karlb/smu
  (setq markdown-command "smu"))
 ((executable-find "markdown_py")
  (setq markdown-command "markdown_py"))
 (t (message "Warning: no markdown program found")))
