(defun whitespace-long-lines (on)
  "Disable displaying long lines in whitespace mode.
With a prefix arg, enable it."
  (interactive "P")
  (if on
      (add-to-list 'whitespace-style 'lines-tail)
    (setq whitespace-style (delete 'lines whitespace-style))
    (setq whitespace-style (delete 'lines-tail whitespace-style)))
  (when (my-interactive-p)
    ;; We need to toggle mode to get new setting to take.
    (whitespace-mode 0)
    (whitespace-mode 1)))

(whitespace-long-lines nil)
