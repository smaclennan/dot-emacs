;; -*- lexical-binding: t -*-

(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'prog-mode-hook #'elide-head-mode)
(when (would-like 'ws-butler)
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(show-paren-mode)

;; Bold SAM comments for simple modes (i.e. use #)
(dolist (mode '(m4-mode makefile-mode makefile-gmake-mode python-mode sh-mode))
  (comment-warn mode))
