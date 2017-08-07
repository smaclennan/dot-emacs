(defun my-byte-compile-buffer ()
  "Byte compile and load the current buffer.
If `compilation-ask-about-save' is nil, saves the file without asking."
  (interactive)
  (require 'compile)
  (save-some-buffers (not compilation-ask-about-save))
  (emacs-lisp-byte-compile-and-load))

;; Redefine compile
(define-key emacs-lisp-mode-map [f7] 'my-byte-compile-buffer)

;; greedy-delete
(when (would-like 'greedy-delete)
  (setq gd-indicator-string nil)
  (gd-add-to-mode))

(my-feature-cond
  (emacs
   (add-hook 'emacs-lisp-mode-hook
	     (lambda () (define-key emacs-lisp-mode-map [(return)] 'newline-and-indent)))))
