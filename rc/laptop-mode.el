;; laptop-mode isn't a true mode, but I think it makes sense to keep
;; it in rc.

(defvar laptop-mode nil
  "*Set laptop mode (larger font).
Setting laptop mode to 'auto tries to guess setting.")

(defvar laptop-mode-font "10x20"
  "*The font to use for laptop mode. (XEmacs)")

(defvar laptop-mode-font-size 12
  "*The font size to use for laptop mode. (GNU Emacs)")

(defvar laptop-mode-fixup nil
  "Do we need to fixup the laptop-mode-font? (XEmacs)")

(when (eq laptop-mode 'auto)
  (if (eq window-system 'x)
      (let ((out (shell-command-to-string "xrandr -q"))
	    (count 0))
	(while (string-match "^[^ ]+ connected" out)
	  (setq count (1+ count))
	  (setq out (replace-match "" nil nil out)))
	(setq laptop-mode (eq count 1)))
    (setq laptop-mode nil)))

(and running-xemacs laptop-mode (setq laptop-mode-fixup t))

(defun laptop-mode-toggle ()
  "Toggle laptop-mode. Doesn't work that well on XEmacs :("
  (interactive)
  (setq laptop-mode (not laptop-mode))
  (let ((font (if laptop-mode laptop-mode-font "7x13")))
    (dolist (face '(default bold italic bold-italic))
      (set-face-font face font))))

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
