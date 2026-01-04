;; -*- lexical-binding: t -*-
;;;###autoload
(defun butterfly ()
  (interactive)
  (unless (and
	   (equal (read-key-sequence "C-x M-c M-b") "u")
	   (equal (read-key-sequence "C-x M-c M-bu") "t")
	   (equal (read-key-sequence "C-x M-c M-but") "t")
	   (equal (read-key-sequence "C-x M-c M-butt") "e")
	   (equal (read-key-sequence "C-x M-c M-butte") "r")
	   (equal (read-key-sequence "C-x M-c M-butter") "f")
	   (equal (read-key-sequence "C-x M-c M-butterf") "l")
	   (equal (read-key-sequence "C-x M-c M-butterfl") "y")
	   (message "I hope you know what you are doing"))
    (error "sorry")))

;; Put this key binding somewhere
;; (global-set-key (kbd "C-x M-c M-b") `butterfly)
