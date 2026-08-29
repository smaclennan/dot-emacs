;; -*- lexical-binding: t; no-byte-compile: t -*-
;; m4-mode inherits from prog-mode
(add-hook 'm4-mode-hook (lambda () (setq tab-width 4)))

;; Not sure why comment-warn did not work... this does
(setq m4-font-lock-keywords
      (append m4-font-lock-keywords '(("\\<dnl SAM\\>.*$" (0 'font-lock-comment-warn-face t)))))
