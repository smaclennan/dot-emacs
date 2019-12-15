;; Warning: If you change this binding, change `my-isearch-word-forward'
(define-key isearch-mode-map "\C-w"		'my-isearch-yank-word)

(define-key isearch-mode-map [f3]		'isearch-repeat-forward)
(define-key isearch-mode-map [(shift f3)]	'isearch-repeat-backward)
(define-key isearch-mode-map "\C-t"		'isearch-toggle-case-fold)

(defun my-isearch-word-forward ()
  "Search for current word."
  (interactive)
  ;; Push the C-w and call 'isearch-forward'
  (setq unread-command-events (listify-key-sequence "\C-w"))
  (isearch-forward))

(defun my-isearch-yank-word ()
  "I don't like how `isearch-yank-word' defines a word, so I rolled my own."
  (interactive)
  (isearch-yank-string (current-word)))
