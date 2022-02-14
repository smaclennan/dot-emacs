;; If you use dwm and tabbed Emacs, you want to make the ediff frame floating:
;; Add to rules[]
;; { "Emacs",    "Ediff",    NULL,       0,            1,           -1 },
;; gtk, of course, does not give the proper instance name because it
;; knows better than you. So for dwm build an X version of Emacs.

(defun my-ediff-quit ()
  "Ediff seems to always leave the bottom buffer selected but I
want the top buffer."
  (let ((top-window (frame-first-window (selected-frame))))
    (while (not (eq (selected-window) top-window))
      (other-window 1))))
(add-hook 'ediff-quit-hook 'my-ediff-quit t)
