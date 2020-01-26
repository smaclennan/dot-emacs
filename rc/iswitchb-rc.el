;; You cannot just add key definitions to `iswitchb-mode-map' since
;; `iswitchb-read-buffer' creates a new map every time it is called.
;; So instead you add to this hook that is run after the new map is
;; created.
(add-hook 'iswitchb-define-mode-map-hook
	  (lambda ()
	    (define-key iswitchb-mode-map [right] 'iswitchb-next-match)
	    (define-key iswitchb-mode-map [left]  'iswitchb-prev-match)))

;; Non-standard key sequence
(global-set-key "\C-x\C-b" 'iswitchb-buffer)
