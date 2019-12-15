(setq ispell-silently-savep t)

(unless ispell-really-hunspell
  ;; hunspell doesn't have this option
  (setq ispell-extra-args '("-W" "3")))

;; This is too early to use ispell-dictionary-alist
;; Assume canadian exists...
(setq ispell-dictionary "canadian")
