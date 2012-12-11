;;; ----------------------------------------------
;; oo-browser

(defun oo-browser-start ()
  (interactive)
  (my-package-cond
   (oo-browser
    (dolist (path load-path)
      (if (string-match "/oo-browser/$" path)
	  (append-to-list 'load-path (concat path "hypb/"))))
    (require 'br-start)
    (global-set-key "\C-c\C-o" 'oo-browser)
    (oo-browser))
   (t (error "oo-browser not installed."))))
(global-set-key "\C-c\C-o" 'oo-browser-start)

