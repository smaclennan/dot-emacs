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

;; Let's try making - part of a "word".
(modify-syntax-entry ?- "w" lisp-mode-syntax-table)
(modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)

;; Bold SAM comments
(require 'font-lock) ; lisp mode called very early in startup

(let ((keyword '((";+ ?\\<SAM\\>.*" 0 'font-lock-comment-warn-face t))))
  (my-feature-cond
    (xemacs
     (nconc lisp-font-lock-keywords-1 keyword)
     (nconc lisp-font-lock-keywords-2 keyword))
  (t
   (font-lock-add-keywords 'emacs-lisp-mode keyword))))
