;; See font-lock.el for a description of why it is called font lock.

;; Maximum colour but minimum chatter
(setq-default font-lock-maximum-decoration t
	      font-lock-verbose nil
	      font-lock-maximum-size nil)

;; -------------------------------------------------------------------------
;; font-lock-comment-warn
;; I want all comments with my initials (SAM) at the start to be very bold
(defface font-lock-comment-warn-face
  '((((class color))  (:foreground "red" :bold t :italic t))
    (((class grayscale) (background light))
     (:foreground "DimGray" :bold t :italic t))
    (((class grayscale) (background dark))
     (:foreground "LightGray" :bold t :italic t))
    (t (:bold t)))
  "Font Lock mode face used to highlight warning comments."
  :group 'font-lock-faces)

;; Change a couple of faces
(make-face-bold 'font-lock-function-name-face)
(set-face-foreground 'font-lock-function-name-face "blue")
;; Um, this is the default
;(set-face-foreground 'font-lock-string-face "green4")

;; Emacs calls font-lock before window-config
(unless (boundp 'laptop-mode)
  (load (concat dot-dir "rc/window-config")))

(if window-system
    (progn
      (set-face-foreground 'font-lock-comment-face "FireBrick")
      (when laptop-mode
	(dolist (face '(font-lock-function-name-face
			font-lock-comment-warn-face
			font-lock-warning-face))
	  (set-face-font face laptop-mode-font))))
  ;; Consoles have less colors to play with
  (set-face-foreground 'font-lock-comment-face "red")
  (set-face-foreground 'font-lock-string-face "green")
  (set-face-foreground 'font-lock-keyword-face "blue")
  (set-face-foreground 'font-lock-variable-name-face "purple")
  )

(if running-xemacs
    (progn
      ;; Of all the modes, font-lock *least* needs a modeline
      ;; indicator. If the buffer is colourful, font-lock is on.
      ;; The only thing you lose is the ability to toggle it.
      (let ((el (assq 'font-lock-mode minor-mode-alist)))
	(if el (setcdr el '(""))))

      (defun setup-font-lock-keywords ()
	(let ((c-regexp "\\(/\\*\\|//\\) ?\\(\\<SAM\\>\\)"))
	  (setq c-font-lock-keywords-1
		(append c-font-lock-keywords-1
			(list (list c-regexp 2 'font-lock-comment-warn-face t))))
	  (setq c-font-lock-keywords-2
		(append c-font-lock-keywords-2
			(list (list c-regexp 2 'font-lock-comment-warn-face t))))
	  (setq c-font-lock-keywords-3
		(append c-font-lock-keywords-3
			(list (list c-regexp 2 'font-lock-comment-warn-face t))))

	  (setq c++-font-lock-keywords-1
		(append c++-font-lock-keywords-1
			(list (list c-regexp 2 'font-lock-comment-warn-face t))))
	  (setq c++-font-lock-keywords-2
		(append c++-font-lock-keywords-2
			(list (list c-regexp 2 'font-lock-comment-warn-face t))))
	  (setq c++-font-lock-keywords-3
		(append c++-font-lock-keywords-3
			(list (list c-regexp 2 'font-lock-comment-warn-face t))))
	  (when nil ;; SAM NOT YET
	    (setq go-mode-font-lock-keywords
		  (append go-mode-font-lock-keywords
			  (list (list c-regexp 2 'font-lock-comment-warn-face t))))
	    ) ;; SAM
	  ))

      (let ((lisp-regexp "; ?\\(\\<SAM\\>\\)"))
	(setq lisp-font-lock-keywords-1
	      (append lisp-font-lock-keywords-1
		      (list (list lisp-regexp 1 'font-lock-comment-warn-face t))))
	(setq lisp-font-lock-keywords-2
	      (append lisp-font-lock-keywords-2
		      (list (list lisp-regexp 1 'font-lock-comment-warn-face t))))
	))

  ;; GNU emacs
  ;; SAM This *should* work for XEmacs too since XEmacs supports
  ;; font-lock-add-keywords but even the example doesn't work.
  (defun setup-font-lock-keywords ()
    (font-lock-add-keywords
     'c-mode
     '(("\\(/\\*\\|//\\) ?\\(\\<SAM\\>\\)" 2 'font-lock-comment-warn-face t)))
    (font-lock-add-keywords
     'c++-mode
     '(("\\(/\\*\\|//\\) ?\\(\\<SAM\\>\\)" 2 'font-lock-comment-warn-face t))))

  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("; ?\\(\\<SAM\\>\\)" 1 'font-lock-comment-warn-face t)))
  (font-lock-add-keywords
   'sh-mode
   '(("# ?\\(\\<SAM\\>\\)" 1 'font-lock-comment-warn-face t)))
  (font-lock-add-keywords
   'makefile-mode
   '(("# ?\\(\\<SAM\\>\\)" 1 'font-lock-comment-warn-face t)))
  )
