
;; Everybody uses mouse-1... except xcscope
(define-key cscope-list-entry-keymap [mouse-1]
  'cscope-mouse-select-entry-other-window)

;; I like speed
(setq cscope-command-args "-q")
