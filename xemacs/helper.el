;; Build helper

(setq dot-dir (expand-file-name "~/.xemacs/"))

(dolist (dir '("lisp" "misc" "xemacs"))
  (add-to-list 'load-path (concat dot-dir dir)))
