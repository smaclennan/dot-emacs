;; Yes, Emacs has a menu bar in console mode
(menu-bar-mode -1)

(setq visible-bell t)

(defvar console-white t
  "Set to non-nil for a white console")

(when console-white
  ;; for some reason bright-white doesn't always work... even on
  ;; machines that report they have bright-white
  (set-background-color "#FFFFFF")
  (set-face-background 'default "#FFFFFF")
  (set-foreground-color "black")
  (set-face-foreground 'default "black")
  )
