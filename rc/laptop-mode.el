(defvar laptop-mode nil
  "*Set laptop mode (larger font).
Setting laptop mode to 'auto tries to guess setting.")

(defvar laptop-mode-font "10x20"
  "*The font to use for laptop mode.")

(defvar laptop-mode-fixup 'auto
  "*Do we need to fixup the laptop-mode-font?
Setting to 'auto tries to guess the setting.")

(when (eq laptop-mode 'auto)
  (if (eq window-system 'x)
      (let ((out (shell-command-to-string "xrandr -q"))
	    (count 0))
	(while (string-match "^[^ ]+ connected" out)
	  (setq count (1+ count))
	  (setq out (replace-match "" nil nil out)))
	(setq laptop-mode (eq count 1)))
    (setq laptop-mode nil)))

(when (eq laptop-mode-fixup 'auto)
  (setq laptop-mode-fixup (and laptop-mode running-xemacs)))

;; -------------------
;; Laptop Mode Helpers

(my-feature-cond
  (xemacs
   (defun check-faces ()
     (interactive)
     (let (found-one)
       (dolist (face (face-list))
	 (unless (equal (face-font-name face) laptop-mode-font)
	   (setq found-one t)
	   (message "%S %s" face (face-font-name face))))
       (if found-one
	   (message "Check the message log")
	 (message "OK"))))))
