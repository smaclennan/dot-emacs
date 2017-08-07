(my-feature-cond
  (xemacs
   (when laptop-mode
     (set-face-font 'flyspell-incorrect-face laptop-mode-font)
     (set-face-font 'flyspell-duplicate-face laptop-mode-font))))
