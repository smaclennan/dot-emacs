;; Domain specific mail config
(let ((domain-specific-init (concat user-emacs-directory "mail-" domain-name)))
  (when (file-exists-p domain-specific-init)
    (load domain-specific-init)))

