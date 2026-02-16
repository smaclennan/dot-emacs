;; -*-no-byte-compile: t -*-
;; early-init.el is new in 27.1. This shaves a good 1/4 second off the init time.
;; emacs-init-time

;; Too early for 'window-system' to be set
(set-scroll-bar-mode 'right)
(tool-bar-mode 0)
(menu-bar-mode 0)

(setq package-enable-at-startup nil)

(setq warning-suppress-log-types '((files missing-lexbind-cookie)))

;; This needs to be very early and needs my `x-is-remote' hack for
;; slow X forwarding connections
;; Note: x-display-name not set yet
(and (boundp 'x-is-remote)
     (string-match "[^:]+:" (getenv "DISPLAY"))
     (setq x-is-remote t))
