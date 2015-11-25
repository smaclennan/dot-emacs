(setq ispell-silently-savep t)

(unless ispell-really-hunspell
  ;; hunspell doesn't have this option
  (setq ispell-extra-args '("-W" "3")))
