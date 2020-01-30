(setq ispell-silently-savep t)

(unless ispell-really-hunspell
  ;; hunspell doesn't have this option
  (setq ispell-extra-args '("-W" "3")))

;; This is too early to use ispell-dictionary-alist
;; Assume canadian exists...
(unless (string-match "ispell" ispell-program-name)
  (setq ispell-dictionary "canadian"))

(when (eq system-type 'qnxnto)
  ;; For some reason we get a filter not found error from aspell even
  ;; though the filters exist in the correct directory.
  (setq ispell-extra-args (cons "--mode=none" ispell-extra-args)))
