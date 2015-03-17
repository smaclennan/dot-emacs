(global-set-key [f3]		'isearch-repeat-forward)
(global-set-key [(shift f3)]    'isearch-repeat-backward)
(global-set-key [XF86_Switch_VT_3] 'isearch-repeat-backward)
(global-set-key [f9]		'my-isearch-word-forward)
(global-set-key [(shift f9)]    'my-toggle-case-search)
(global-set-key [XF86_Switch_VT_9] 'my-toggle-case-search)

;;  I don't like the way isearch-yank-word defines word, so I rolled my own
(defun my-isearch-yank-word ()
  "Pull current word from buffer into search string.
Use region if it exists. My replacement for isearch-yank-word."
  (interactive)
  (let ((word (if (region-exists-p)
		  (buffer-substring (region-beginning) (region-end))
		(current-word))))
    (forward-char 1) ;; make sure we are not on first char of word
    (if (fboundp 'isearch-yank)
	(isearch-yank word)
      (isearch-yank-string word))))

;; Warning: If you change this binding, change `my-isearch-word-forward'
(define-key isearch-mode-map "\C-w"		'my-isearch-yank-word)

(define-key isearch-mode-map [f3]		'isearch-repeat-forward)
(define-key isearch-mode-map [(shift f3)]	'isearch-repeat-backward)
(define-key isearch-mode-map "\C-t"		'isearch-toggle-case-fold)

(defun my-isearch-word-forward (&optional regexp-p)
  "Search for current word. Region is used if set."
  (interactive "P")
  ;; Push the C-w and call 'isearch-forward'
  (setq unread-command-events
	(if running-xemacs
	    (list (make-event 'key-press '(key ?w modifiers (control))))
	  (listify-key-sequence "\C-w")))
  (isearch-mode t (not (null regexp-p)) nil (not (interactive-p))))

(defun my-toggle-case-search ()
  (interactive)
  (setq case-fold-search (not case-fold-search))
  (when (interactive-p)
    (message "Case sensitive search %s." (if case-fold-search "off" "on"))))
