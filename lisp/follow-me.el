;; -*- lexical-binding: t; -*-

(define-minor-mode follow-me-mode
  "Minor mode for clickable links."
  :lighter " fm!"
  :keymap '(([mouse-1] . follow-me)))

;;;###autoload
(defun follow-me (_event)
  (interactive "e")
  (browse-url (thing-at-point 'url)))

;;;###autoload
(defun follow-me-add (begin end &optional display)
  (interactive "r")
  (put-text-property begin end 'face '(:foreground "blue" :underline nil))
  (put-text-property begin end 'mouse-face '(:foreground "blue" :underline t))
  (put-text-property begin end 'display display)
  ;; pointer
  ;; display
  (put-text-property begin end 'keymap follow-me-mode-map))

;; http://seanm.ca/

(provide 'follow-me)
