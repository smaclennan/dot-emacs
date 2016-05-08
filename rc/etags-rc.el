(if running-xemacs
    (setq tags-build-completion-table nil ;; too slow
	  tags-auto-read-changed-tag-files t)
  (setq tags-revert-without-query t))
