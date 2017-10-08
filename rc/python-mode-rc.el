;; Bold SAM comments
(let ((keyword '(("# ?\\<SAM\\>.*" 0 'font-lock-comment-warn-face t))))
  (my-feature-cond
    (xemacs
     (nconc python-font-lock-keywords keyword))
    (t
     (font-lock-add-keywords 'python-mode keyword))))
