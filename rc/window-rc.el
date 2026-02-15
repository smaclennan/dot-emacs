;; -*- lexical-binding: t -*-
;; Called for window systems and console mode

(setq frame-title-format '("Emacs " emacs-version " " system-name ":"
			   (buffer-file-name
			    (:eval (abbreviate-file-name buffer-file-name))
			    "%b")))

(setq use-dialog-box nil)
(menu-bar-mode 0)
(delete-selection-mode)

;; Disable tooltip output...
;; First the popups, then the text
(tooltip-mode 0)
(setq show-help-function nil)

(blink-cursor-mode 0)
(set-cursor-color "red")

(setq visible-bell t)

(defvar console-white t "Set to non-nil for a white console")

(defun my-display-buffer-at-bottom (buffer alist)
  "Try to keep all '*<name>*' windows at the bottom.
1. If we find the buffer in a window, use it (prefer bottom).
2. If there is a bottom window (i.e more than one), use it.
3. Split the current window and use the bottom one.
"
  (let (saw-window found bottom)
    (walk-window-tree
     (lambda (window)
       (when (eq (window-buffer window) buffer)
	 (setq found window))
       (when saw-window
	 (setq bottom window))
       (setq saw-window t))
     nil nil 'nomini)
    (if found
	;; Exact match - use it
	(window--display-buffer buffer found 'reuse alist)
      (if bottom
	  ;; At least two windows, use the bottom one
	  (window--display-buffer buffer bottom 'reuse alist)
	;; One window, let this do the heavy work of splitting
	(display-buffer-at-bottom buffer alist)))))

(setq display-buffer-alist '(("^\*" my-display-buffer-at-bottom)))

(if window-system
    (progn
      (set-scroll-bar-mode 'right)

      (tool-bar-mode 0)

      ;; Less aggressive tool tips
      (setq tooltip-delay 2)	  ; slower to start
      (setq tooltip-hide-delay 2) ; faster to go

      (global-set-key [(control insert)] 'clipboard-kill-ring-save)
      (global-set-key [(shift insert)] 'clipboard-yank)

      (setq confirm-kill-emacs `y-or-n-p))
  (setq visable-cursor nil) ;; Needed for, at least, urxvt
  (when console-white
    ;; for some reason bright-white doesn't always work... even on
    ;; machines that report they have bright-white
    (set-foreground-color "black")
    (set-face-foreground 'default "black")
    (set-background-color "#FFFFFF")
    (set-face-background 'default "#FFFFFF")))

(with-no-warnings (iswitchb-mode))

;; --------------------------------------------
;; laptop mode

(defvar laptop-mode nil
  "When non-nil set laptop mode (`laptop-mode-font-size').
With X, setting laptop mode to \\='auto turns on laptop mode if
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
