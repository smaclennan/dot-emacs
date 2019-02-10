;; So makefiles get nice compile commands
(require 'my-compile)

(comment-warn 'makefile-mode)
(comment-warn 'makefile-gmake-mode)

(add-hook 'makefile-mode-hook 'flyspell-prog-mode)
