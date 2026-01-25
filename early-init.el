;; -*-no-byte-compile: t -*-
;; early-init.el is new in 27.1. This shaves a good 1/4 second off the init time.
;; emacs-init-time

(when window-system (tool-bar-mode 0))
(menu-bar-mode 0)

(setq package-enable-at-startup nil)

(setq warning-suppress-log-types '((files missing-lexbind-cookie)))

;; This needs to be very early and needs my `x-is-remote' hack for
;; slow X forwarding connections
;; Note: x-display-name not set yet
(let ((x-display-name (getenv "DISPLAY")))
  (and (boundp 'x-is-remote)
       (string-match "[^:]+:" x-display-name)
       (setq x-is-remote t)))
