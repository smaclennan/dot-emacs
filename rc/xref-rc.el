(defun xref-find-definitions-prompt ()
  "Same as `xref-find-definitions' except it always prompts for
the identifier."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'xref-find-definitions)))
