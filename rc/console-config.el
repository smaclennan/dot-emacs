;; Yes, Emacs has a menu bar in console mode
(menu-bar-mode -1)

(defvar console-white t
  "Set to non-nil for a white console")

(when console-white
  (set-background-color "brightwhite")
  (set-face-background 'default "brightwhite")
  (set-foreground-color "black")
  (set-face-foreground 'default "black")
  )
