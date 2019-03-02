(add-to-list 'load-path (concat user-emacs-directory "compat"))
(load "compat-loaddefs" t)

;; Back before GNU Emacs went insane about common lisp
(require 'cl)

(defalias 'cl-remove-if 'remove-if)
(defalias 'cl-caddr 'caddr)
(defalias 'cl-cdadr 'cdadr)

(provide 'cl-extra)
