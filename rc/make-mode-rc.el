;; This is required so that make-clean will work
(require 'compile)

;; Bold SAM comments
;; SAM not working for GNU Emacs
(comment-warn (list makefile-font-lock-keywords) 'makefile-mode)
