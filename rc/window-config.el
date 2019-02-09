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

(when (eq window-system 'x)
  (my-feature-cond
   (clipboard-kill-ring-save ;; 25.1.1 has this
    (global-set-key [(control insert)] 'clipboard-kill-ring-save))
   (t
    (defun my-clipboard-copy (beg end)
      (interactive "r")
      (let ((text (buffer-substring beg end)))
	(x-set-selection 'CLIPBOARD text) ;; for C-v
	(x-set-selection 'PRIMARY text)) ;; for mouse paste
      (copy-region-as-kill beg end)) ;; and the kill buffer
    (global-set-key [(control insert)] 'my-clipboard-copy)))

  (global-set-key [(shift insert)] 'x-clipboard-yank))

;; -------------------------------------------------------
;; The standard blows away emacs just a little to easily
(defun my-save-buffers-kill-emacs ()
  (interactive)
  (if (y-or-n-p "Do you have to go? ") (save-buffers-kill-emacs)))

(global-set-key "\C-x\C-c" 'my-save-buffers-kill-emacs)

;; -------------------------------------------------------
;; intellimouse i.e. scroll wheel

(defvar intellimouse-scroll-lines 4 "*Number of lines to scroll")

(defun intellimouse-scroll-down (event)
  "Mouse scroll wheel down"
  (interactive "e")
  (with-selected-window (cl-caadr event)
    (scroll-down intellimouse-scroll-lines)))

(defun intellimouse-scroll-up (event)
  "Mouse scroll wheel up"
  (interactive "e")
  (with-selected-window (cl-caadr event)
    (scroll-up intellimouse-scroll-lines)))

(global-set-key [(mouse-4)] 'intellimouse-scroll-down)
(global-set-key [(mouse-5)] 'intellimouse-scroll-up)

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
  (setq laptop-mode
	(if (eq window-system 'x)
	    (eq 1 (with-temp-buffer
		    (call-process "xrandr" nil t nil "-q")
		    (count-matches "^[^ ]+ connected" 0 (point-max))))
	  nil)))

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
