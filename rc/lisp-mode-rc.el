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

(comment-warn (list lisp-font-lock-keywords-1 lisp-font-lock-keywords-2)
	      'emacs-lisp-mode
	      ";+ ?\\<SAM\\>.*")

;; I don't like the warning-face used in GNU Emacs for functions like `error'.
;; However, the keywords are a defconst, so we must work around that by
;; making a copy. XEmacs doesn't consider error a keyword.

(my-feature-cond
  (xemacs
   (nconc lisp-font-lock-keywords-2 '(("(\\(error\\|warn\\)\\>" 1 'font-lock-keyword-face))))
  (t
   (let ((mine (copy-tree lisp-el-font-lock-keywords-2))
	 str)
     (dolist (face mine)
       (setq str (car face))
       (and (stringp str)
	    (string-match "error" str)
	    (setf (cdadr face) (list 'font-lock-keyword-face))))
     (setq lisp-el-font-lock-keywords-2 mine))))
