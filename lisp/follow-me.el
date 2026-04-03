;; -*- lexical-binding: t; -*-

;; Simple minor mode for clickable links
;;
;; Call `follow-me-add' with a region that contains an url. The region
;; will be shown as blue and underlined when the mouse is over it.
;;
;; mouse-1, or return, will send the url to your default browser.
;; mouse-3 will copy the url to the clipboard for cut and paste.

(define-minor-mode follow-me-mode
  "Minor mode for clickable links."
  :lighter " fm!"
  :keymap '(([mouse-1] . follow-me-mouse)
	    ([mouse-3] . follow-me-copy)
	    ([return]  . follow-me)
	    ))

;;;###autoload
(defun follow-me-add (begin end &optional display)
  (interactive "r")
  (put-text-property begin end 'face '(:foreground "blue" :underline nil))
  (put-text-property begin end 'mouse-face '(:foreground "blue" :underline t))
  (put-text-property begin end 'display display)
  ;; pointer
  ;; display
  (put-text-property begin end 'keymap follow-me-mode-map))

;;;###autoload
(defun follow-me ()
  (interactive)
  (browse-url (thing-at-point 'url)))

;;;###autoload
(defun follow-me-mouse (_event)
  (interactive "e")
  (browse-url (thing-at-point 'url)))

;;;###autoload
(defun follow-me-copy (event)
  (interactive "e")
  (mouse-set-point event)
  (let ((url (thing-at-point 'url))
	(select-enable-clipboard t)
	(select-enable-primary t))
    (gui-select-text url)))

;; http://seanm.ca/

(provide 'follow-me)
