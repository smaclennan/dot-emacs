;; Windowing system only config.
;; No real library to toggle off of so it is not a -rc.el

;; I believe for Emacs system-name is host name only
(setq frame-title-format '("Emacs " emacs-version " " system-name ":"
			   (buffer-file-name
			    (:eval (abbreviate-file-name buffer-file-name))
			    "%b")))

(setq use-dialog-box nil)
(tool-bar-mode 0)
;; Tooltips can hang emacs over VPN
;;(tooltip-mode 0)
;;(display-time)

;; Set the cursor properly for Emacs
(blink-cursor-mode 0)
(set-cursor-color "red")

(global-set-key [(control insert)] 'clipboard-kill-ring-save)
(global-set-key [(shift insert)] 'clipboard-yank)

(defadvice save-buffers-kill-emacs (before ask-first activate)
  (y-or-n-p "Do you have to go? "))

;; --------------------------------------------
;; laptop mode

(defvar laptop-mode nil
  "When non-nil set laptop mode (`laptop-mode-font-size').
With X, setting laptop mode to 'auto turns on laptop mode if
you have only one monitor.")

(defvar laptop-mode-font-size 120
  "The font size to use for laptop mode in pixels.
When a list, it is the sizes for normal and laptop
mode. Generally you don't need to setup the list, laptop-mode
will do it for you.")

(when (eq laptop-mode 'auto)
  (setq laptop-mode
	(and (eq window-system 'x)
	     (eq 1 (with-temp-buffer
		     (call-process "xrandr" nil t nil "-q")
		     (count-matches "^[^ ]+ connected" (point-min)))))))

(defun laptop-mode-toggle (&optional on)
  "Toggle laptop-mode."
  (interactive)
  (setq laptop-mode (if on t (not laptop-mode)))
  (unless (listp laptop-mode-font-size)
    (setq laptop-mode-font-size (list (face-attribute 'default :height)
				      laptop-mode-font-size)))
  (set-face-attribute 'default nil :height
		      (if laptop-mode
			  (nth 1 laptop-mode-font-size)
			(nth 0 laptop-mode-font-size))))

(when laptop-mode (laptop-mode-toggle t))
