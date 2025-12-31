;; -*-no-byte-compile: t -*-
;; early-init.el is new in 27.1. This shaves a good 1/4 second off the init time.
;; emacs-init-time

(when window-system (tool-bar-mode 0))
(menu-bar-mode 0)

(setq package-enable-at-startup nil)

(setq warning-suppress-log-types '((files missing-lexbind-cookie)))
