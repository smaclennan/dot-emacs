;; Ediff 1.76 bug - coding system was set to 'emacs-internal which
;; doesn't seem to exist. You see it with ediff-buffers but not
;; ediff-files. Just set it back to no-conversion.
(setq ediff-coding-system-for-write 'no-conversion)

;; Needed by ediff - exists in `efs'
(or (boundp 'allow-remote-paths) (setq allow-remote-paths nil))

(defun my-set-face (face fg bg &optional prop)
  (set-face-foreground face fg)
  (set-face-background face bg)
  (when prop (set-face-property face prop t)))

(unless window-system
  (when (or (featurep 'xemacs) (>= emacs-major-version 22))
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

;; -------------------
(when nil ;; SAM doesn't seem to be needed any more
;; Hack to put the ediff control in the window (rather than off it)
;; Needed for window manager like PWM that do not honor the window move
;; request. The window will look strange until XEmacs updates it.
(defvar ediff-control-frame-position (list (cons 'top 10) (cons 'left 10))
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
