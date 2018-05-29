;; display-time

(my-feature-cond
  (xemacs
   (when laptop-mode
     (setq display-time-show-icons-maybe nil))
   )
  (t ;; want angry mail
   (setq display-time-use-mail-icon t)
   (defface angry-mail-face '((t (:background "red")))
     "Make the mail face angry."
     :group 'display-time)
   (setq display-time-mail-face 'angry-mail-face)))
