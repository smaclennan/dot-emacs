(my-feature-cond
  (emacs
   (font-lock-add-keywords
    'sh-mode
    '(("# ?\\<SAM\\>.*" 0 'font-lock-comment-warn-face t)))
   (add-hook 'sh-mode-hook
	     (lambda () (define-key sh-mode-map [(return)] 'newline-and-indent)))))
