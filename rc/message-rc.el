(when (fboundp 'bbdb-initialize)
  (bbdb-initialize 'message)
  ;; M-tab taken by flyspell
  (define-key message-mode-map [(meta insert)] 'bbdb-complete-mail)
  )
