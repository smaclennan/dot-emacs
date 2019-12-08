;; From hide-copyleft.el on which elide-head is based:
;; If you're sure you're not gonna get sued, you can do something like this
;; in your .emacs file.

(nconc elide-head-headers-to-hide
       '((" \\* The Apache Software License, Version 1\\.1" . " \\*/")
	 (" \\* \\$QNXLicenseC:" . " \\* \\$")))
