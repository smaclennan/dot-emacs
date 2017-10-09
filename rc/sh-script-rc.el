(my-feature-cond
  (emacs
   (add-hook 'sh-mode-hook
	     (lambda () (define-key sh-mode-map [(return)] 'newline-and-indent)))))

;; Bold SAM comments
;; SAM not working for XEmacs
;(comment-warn (list sh-font-lock-keywords) 'sh-mode)
(comment-warn nil 'sh-mode)
