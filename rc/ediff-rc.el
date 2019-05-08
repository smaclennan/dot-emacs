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
(setf (alist-get 'top ediff-control-frame-parameters) 1)
(setf (alist-get 'left ediff-control-frame-parameters) 856)
