;; early-init.el is new in 27.1. This shaves a good 0.250 seconds off the init time.

(when window-system (tool-bar-mode 0))
(menu-bar-mode 0)

(setq package-enable-at-startup nil)
