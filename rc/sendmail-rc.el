;; Domain specific mail config

(defvar domain-name
  (let ((my-system-name (system-name)))
    (if (string-match "^\\([^.]+\\)\\.\\(.*\\)" my-system-name)
	;; fully qualified system-name
	(match-string 2 my-system-name)
      (getenv "DOMAINNAME"))))

(let ((domain-specific-init (concat user-emacs-directory "mail-" domain-name)))
  (if (file-exists-p domain-specific-init)
      (load domain-specific-init)
    (load "mail" t)))
