;; Called for window systems and console mode

(setq frame-title-format '("Emacs " emacs-version " " system-name ":"
			   (buffer-file-name
			    (:eval (abbreviate-file-name buffer-file-name))
			    "%b")))

(setq use-dialog-box nil)
(menu-bar-mode 0)
(delete-selection-mode)

(blink-cursor-mode 0)
(set-cursor-color "red")

(setq visible-bell t)

(defvar console-white t "Set to non-nil for a white console")

(if window-system
    (progn
      (tool-bar-mode 0)
      
      (global-set-key [(control insert)] 'clipboard-kill-ring-save)
      (global-set-key [(shift insert)] 'clipboard-yank)

      (defadvice save-buffers-kill-emacs (before ask-first activate)
	(y-or-n-p "Do you have to go? ")))
  (when console-white
    ;; for some reason bright-white doesn't always work... even on
    ;; machines that report they have bright-white
    (set-background-color "#FFFFFF")
    (set-face-background 'default "#FFFFFF")
    (set-foreground-color "black")
    (set-face-foreground 'default "black")))

;; --------------------------------------------
;; iswitchb - yes `switch-to-buffer' is considered a window function.
;; This gets rid of the iswitchb deprecated message by moving it out
;; of the obsolete directory.
(let ((to (concat user-emacs-directory "lisp/iswitchb.elc")))
  (unless (file-exists-p to)
    (make-symbolic-link (locate-library "iswitchb") to t)))
(with-no-warnings (iswitchb-mode))

;; --------------------------------------------
;; laptop mode

(defvar laptop-mode nil
  "When non-nil set laptop mode (`laptop-mode-font-size').
With X, setting laptop mode to 'auto turns on laptop mode if
you have only one monitor.")

(defvar laptop-mode-font-size
  ;; Input Mono seems larger than other fonts
  (if (and window-system (string-match "Input Mono" (face-font 'default)))
      110
    120)
  "The font size to use for laptop mode in pixels.
When a list, it is the sizes for normal and laptop
mode. Generally you don't need to setup the list, laptop-mode
will do it for you.")

(when (eq laptop-mode 'auto)
  (setq laptop-mode
	(and (eq window-system 'x)
	     (eq 1 (with-temp-buffer
		     (call-process "xrandr" nil t nil "-q")
		     (count-matches "^[^ ]+ connected"
				    (point-min) (point-max)))))))

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
