;; SAM Most of this can go in init.el and some to sam-common.el

;; When building outside emacs dot-dir may not be set
(defvar dot-dir (expand-file-name "~/.emacs.d/"))

(dolist (dir '("lisp" "misc"))
  (add-to-list 'load-path (concat dot-dir dir))
  (load (concat dir "-loaddefs") t t))

;; I don't know why the hate against common-lisp
(setq byte-compile-warnings '(not cl-functions))
(require 'cl)
(require 'cl-extra)
(require 'ring)
(require 'sam-common)
