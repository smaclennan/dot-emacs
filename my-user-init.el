;; Start *after* display-time
(when (and window-system (not running-as-root) (would-like 'slashdot))
  (slashdot-start))
