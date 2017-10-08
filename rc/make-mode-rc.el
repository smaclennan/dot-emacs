;; This is required so that make-clean will work
(require 'compile)

;; Bold SAM comments
(let ((keyword '(("# ?\\<SAM\\>.*" 0 'font-lock-comment-warn-face t))))
  (my-feature-cond
    (xemacs
     (nconc makefile-font-lock-keywords keyword))
    (t
     (font-lock-add-keywords 'makefile-mode keyword))))
