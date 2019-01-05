;; Windowing system only config.
;; No real library to toggle off of so it is not a -rc.el

(defvar bells-and-whistles nil "*If non-nil, enable all graphical widgets.")

(setq frame-title-format '("Emacs " emacs-version " " host-name ":"
			   (buffer-file-name
			    (:eval (abbreviate-file-name buffer-file-name))
			    "%b")))

(if (fboundp 'delete-selection-mode)
    (delete-selection-mode)
  (and (< emacs-major-version 23)
       (would-like 'pc-select)
       (pc-selection-mode)))

(unless bells-and-whistles
  (setq use-dialog-box nil)
  ;; Toolbar
  (tool-bar-mode 0)
  ;; Tooltips hang emacs over VPN
  (tooltip-mode 0))

;; Set the cursor properly for Emacs
(blink-cursor-mode 0)
(set-cursor-color "red")

;; Do this *after* setting the modeline colours
(and nil (fboundp 'display-time) (display-time))

;; --------------------------------------------
;; laptop mode

(defvar laptop-mode nil
  "*Set laptop mode (larger font).
Setting laptop mode to 'auto tries to guess setting.")

(defvar laptop-mode-font-size 120
  "*The font size to use for laptop mode in pixels.
When a list, it is the sizes for normal and laptop
mode. Generally you don't need to setup the list, laptop-mode
will do it for you.")

(when (eq laptop-mode 'auto)
  (if (eq window-system 'x)
      (let ((out (shell-command-to-string "xrandr -q"))
	    (count 0))
	(while (string-match "^[^ ]+ connected" out)
	  (setq count (1+ count))
	  (setq out (replace-match "" nil nil out)))
	(setq laptop-mode (eq count 1)))
    (setq laptop-mode nil)))

(unless (listp laptop-mode-font-size)
  (setq laptop-mode-font-size (list (face-attribute 'default :height)
				    laptop-mode-font-size)))

(defun laptop-mode-toggle (&optional on)
  "Toggle laptop-mode."
  (interactive)
  (setq laptop-mode (if on t (not laptop-mode)))
  (set-face-attribute 'default nil :height
		      (if laptop-mode
			  (nth 1 laptop-mode-font-size)
			(nth 0 laptop-mode-font-size))))

(when laptop-mode (laptop-mode-toggle t))
