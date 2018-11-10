;; Ediff 1.76 bug - coding system was set to 'emacs-internal which
;; doesn't seem to exist. You see it with ediff-buffers but not
;; ediff-files. Just set it back to no-conversion.
(setq ediff-coding-system-for-write 'no-conversion)

;; Needed by ediff - exists in `efs'
(or (boundp 'allow-remote-paths) (setq allow-remote-paths nil))

(defun my-set-face (face fg bg &optional highlight)
  (set-face-foreground face fg)
  (set-face-background face bg)
  (when highlight
    (if running-xemacs
	(set-face-property face 'highlight t)
      (set-face-attribute face nil :weight 'bold))))

(unless window-system
  (when (or running-xemacs (>= emacs-major-version 22))
    (my-set-face 'ediff-current-diff-A "black" "yellow")
    (my-set-face 'ediff-current-diff-B "black" "yellow")
    (my-set-face 'ediff-current-diff-C "black" "yellow")
    (my-set-face 'ediff-fine-diff-A    "red"   "yellow")
    (my-set-face 'ediff-fine-diff-B    "red"   "yellow")
    (my-set-face 'ediff-fine-diff-C    "red"   "yellow")
    (my-set-face 'ediff-odd-diff-A     "black" "white" 'highlight)
    (my-set-face 'ediff-odd-diff-B     "black" "white" 'highlight)
    (my-set-face 'ediff-even-diff-A    "black" "white" 'highlight)
    (my-set-face 'ediff-even-diff-B    "black" "white" 'highlight)
    ))

(defun my-ediff-quit ()
  "Ediff seems to always leave the bottom buffer selected but I
want the top buffer."
  (let ((top-window (frame-first-window (selected-frame))))
    (while (not (eq (selected-window) top-window))
      (other-window 1))))
(add-hook 'ediff-quit-hook 'my-ediff-quit t)

;; -------------------
(when (not running-xemacs)
  ;; Hack to put the ediff control in the right place. If we don't do
  ;; this then the window loses focus under dwm.
  (setf (alist-get 'top ediff-control-frame-parameters) 1)
  (setf (alist-get 'left ediff-control-frame-parameters) 856)
  )

(when nil ;; SAM doesn't seem to be needed any more
;; Needed for window manager like PWM that do not honor the window move
;; request. The window will look strange until XEmacs updates it.
(defvar ediff-control-frame-position (list (cons 'top 1) (cons 'left 856))
  "* Where to put the control frame on the screen.")

(defun ediff-control-frame-hack ()
  (setq ediff-control-frame-parameters
	(remassq 'left
		 (remassq 'top ediff-control-frame-parameters)))
  (setq ediff-control-frame-parameters
	(nconc ediff-control-frame-parameters ediff-control-frame-position)))

(add-hook 'ediff-load-hook 'ediff-control-frame-hack)
)
;; -------------------
