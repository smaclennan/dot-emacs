;; From hide-copyleft.c
;; If you're sure you're not gonna get sued, you can do something like this
;; in your .emacs file.

;; Allow both semi-colon and colon
(mapc (lambda (one)
	(when (string-match "^free software\\(;\\)" (car one))
	  (setcar one (replace-match "[;:]" nil t (car one) 1))))
      copylefts-to-hide)

(nconc copylefts-to-hide
       '((" \\* The Apache Software License, Version 1\\.1" . " \\*/")
	 (" \\* \\$QNXLicenseC:" . " \\* \\$")))
