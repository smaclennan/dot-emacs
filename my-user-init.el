;; Start *after* display-time
(when (and window-system (not running-as-root) (would-like 'slashdot))
  ;; madpenguin.org does not work if User-Agent is Wget
  (setq slashdot-wget-options (append http-wget-options '("-A" "Mozilla")))
  (slashdot-start))
