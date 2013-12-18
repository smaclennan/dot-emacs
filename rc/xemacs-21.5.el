;; For some reason the file coding was gutted - put it back
(setq buffer-file-coding-system-for-read 'undecided
      default-buffer-file-coding-system  'raw-text)

;; New modeline format does not fit on a smaller window
;(setq-default
; modeline-buffer-id
; (list (cons modeline-buffer-id-left-extent 'modeline-buffer-id-left)))
