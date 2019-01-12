;; This is required so that make-clean will work
(require 'compile)

;; So makefiles get nice compile commands
(require 'my-compile)

(comment-warn 'makefile-mode)
(comment-warn 'makefile-gmake-mode)
