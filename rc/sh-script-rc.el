(add-hook 'sh-mode-hook
	  (lambda () (define-key sh-mode-map [(return)] 'newline-and-indent)))

;; Bold SAM comments
(comment-warn 'sh-mode)
