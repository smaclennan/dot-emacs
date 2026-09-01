;; -*- lexical-binding: t; no-byte-compile: t -*-

(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'prog-mode-hook #'elide-head-mode)
(when (would-like 'ws-butler)
  (add-hook 'prog-mode-hook #'ws-butler-mode))

(show-paren-mode)

;; Bold SAM comments for simple modes (i.e. use #)
(dolist (mode '(m4-mode makefile-mode makefile-gmake-mode python-mode sh-mode))
  (comment-warn mode))

;; Deal with git master vs main
(defvar git-master "master"
  "With the change to main, allow overriding master")

;; Is your git PC?
(defun git-pc ()
  (let ((dot-git (git-dir)))
    (and dot-git
	 (file-exists-p (concat dot-git ".git/refs/heads/main"))
	 (setq-local git-master "main"))))
(add-hook 'prog-mode-hook 'git-pc)
