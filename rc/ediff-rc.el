;; Ediff 1.76 bug - coding system was set to 'emacs-internal which
;; doesn't seem to exist. You see it with ediff-buffers but not
;; ediff-files. Just set it back to no-conversion.
(setq ediff-coding-system-for-write 'no-conversion)

(unless window-system
  (defmacro my-set-face (face fg bg weight)
    `(set-face-attribute ,face nil :foreground ,fg :background ,bg :weight ,weight))
  (my-set-face 'ediff-current-diff-A "black" "yellow" 'normal)
  (my-set-face 'ediff-current-diff-B "black" "yellow" 'normal)
  (my-set-face 'ediff-current-diff-C "black" "yellow" 'normal)
  (my-set-face 'ediff-fine-diff-A    "red"   "yellow" 'normal)
  (my-set-face 'ediff-fine-diff-B    "red"   "yellow" 'normal)
  (my-set-face 'ediff-fine-diff-C    "red"   "yellow" 'normal)
  (my-set-face 'ediff-odd-diff-A     "black" "white" 'bold)
  (my-set-face 'ediff-odd-diff-B     "black" "white" 'bold)
  (my-set-face 'ediff-even-diff-A    "black" "white" 'bold)
  (my-set-face 'ediff-even-diff-B    "black" "white" 'bold)
  )

(defun my-ediff-quit ()
  "Ediff seems to always leave the bottom buffer selected but I
want the top buffer."
  (let ((top-window (frame-first-window (selected-frame))))
    (while (not (eq (selected-window) top-window))
      (other-window 1))))
(add-hook 'ediff-quit-hook 'my-ediff-quit t)

;; -------------------
;; Hack to put the ediff control in the right place. If we don't do
;; this then the window loses focus under dwm.
;; SAM still needed? This is a problem under 24
;;(setf (alist-get 'top ediff-control-frame-parameters) 1)
;;(setf (alist-get 'left ediff-control-frame-parameters) 856)
