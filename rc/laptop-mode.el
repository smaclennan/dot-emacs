;; laptop-mode isn't a true mode, but I think it makes sense to keep
;; it in rc.

(defvar laptop-mode nil
  "*Set laptop mode (larger font).
Setting laptop mode to 'auto tries to guess setting.")

(defvar laptop-mode-font (if (boundp 'xft-version)
			     '("Input Mono-10" "Input Mono-12")
			   '("7x13" "10x20"))
  "*The font to use for laptop mode. (XEmacs).
When a list, it is the fonts to use for normal and laptop
mode.")

(defvar laptop-mode-font-size 120
  "*The font size to use for laptop mode in pixels. (GNU Emacs).
When a list, it is the sizes for normal and laptop
mode. Generally you don't need to setup the list, laptop-mode
will do it for you.")

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

(my-feature-cond
  (emacs
   (unless (listp laptop-mode-font-size)
     (setq laptop-mode-font-size (list (face-attribute 'default :height)
				       laptop-mode-font-size)))))

(defun laptop-mode-toggle (&optional on)
  "Toggle laptop-mode. Doesn't work that well on XEmacs :("
  (interactive)
  (setq laptop-mode (if on t (not laptop-mode)))
  (my-feature-cond
    (xemacs
     (let (font)
       (if (listp laptop-mode-font)
	   (if laptop-mode
	       (setq font (nth 1 laptop-mode-font))
	     (setq font (nth 0 laptop-mode-font)))
	 (setq font (if laptop-mode laptop-mode-font "7x13")))
       (dolist (face '(default bold italic bold-italic))
	 (set-face-font face font)))
     ;; We need a frame redraw after changing the fonts or we get
     ;; artifacts at the bottom of the display
     (redraw-frame nil t))
    (emacs
     (set-face-attribute 'default nil :height
			 (if laptop-mode
			     (nth 1 laptop-mode-font-size)
			   (nth 0 laptop-mode-font-size))))
    ))

(when laptop-mode (laptop-mode-toggle t))

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
