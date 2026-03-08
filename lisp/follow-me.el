;; -*- lexical-binding: t; -*-

(define-minor-mode follow-me-mode
  "Minor mode for clickable links."
  :lighter " fm!"
  :keymap '(([mouse-1] . follow-me)))

(defun follow-me (_event)
  (interactive "e")
  (browse-url (thing-at-point 'url)))

;;;###autoload
(defun follow-me-add (begin end)
  (interactive "r")
  (put-text-property begin end 'face '(:foreground "blue"))
  (put-text-property begin end 'keymap follow-me-mode-map))

(provide 'follow-me)
