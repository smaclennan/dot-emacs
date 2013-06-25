;; Domain specific mail config
(let ((domain-specific-init (concat dot-dir "mail-" domain-name)))
  (when (file-exists-p domain-specific-init)
    (load domain-specific-init)))
