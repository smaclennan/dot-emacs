;; This is required so that make-clean will work
(require 'compile)

;; So makefiles get nice compile commands
(require 'my-compile)

;; Bold SAM comments
(if running-xemacs
    (comment-warn (list makefile-font-lock-keywords) nil)
  (comment-warn nil 'makefile-mode)
  (comment-warn nil 'makefile-gmake-mode)
  )
