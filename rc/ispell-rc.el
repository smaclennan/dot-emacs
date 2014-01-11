(setq ispell-silently-savep t
      ispell-extra-args '("-W" "3"))

;; For aspell
(when (exec-installed-p "aspell")
  (setq-default ispell-program-name "aspell"))
