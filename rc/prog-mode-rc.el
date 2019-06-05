(add-hook 'prog-mode-hook #'ws-butler-mode)

(add-hook 'prog-mode-hook #'flyspell-prog-mode)

(add-hook 'prog-mode-hook #'hide-copyleft-region)

(show-paren-mode t)

;; Bold SAM comments for simple modes (i.e. use #)
(dolist (mode '(m4-mode makefile-mode makefile-gmake-mode python-mode sh-mode))
  (comment-warn mode))
