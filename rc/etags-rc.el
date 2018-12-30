(my-feature-cond
  (xemacs
   (setq tags-build-completion-table nil ;; too slow
	 tags-auto-read-changed-tag-files t))
  (t
   (setq tags-revert-without-query t)
   (setq tags-add-tables t) ;; Don't ask, just do
   ))
