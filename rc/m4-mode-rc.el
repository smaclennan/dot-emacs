;; m4-mode inherits from prog-mode
(defun my-m4-hook () (setq tab-width 4))
(add-hook 'm4-mode-hook 'my-m4-hook)

;; Bold SAM comments
(comment-warn 'm4-mode)
