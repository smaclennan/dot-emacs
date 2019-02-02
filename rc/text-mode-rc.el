(add-hook 'text-mode-hook 'turn-on-auto-fill)

(defun check-for-readme ()
  "If this is a commit buffer, set to text mode."
  (and (eq major-mode 'fundamental-mode)
       (string= (buffer-name) "README")
       (text-mode)))
(add-hook 'find-file-hooks 'check-for-readme t)
