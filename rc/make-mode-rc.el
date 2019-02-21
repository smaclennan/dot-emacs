;; So makefiles get nice compile commands
(require 'my-compile)
(add-hook 'makefile-mode-hook 'my-compile-command t)

(comment-warn 'makefile-mode)
(comment-warn 'makefile-gmake-mode)
