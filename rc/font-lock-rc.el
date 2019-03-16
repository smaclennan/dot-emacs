;; See font-lock.el for a description of why it is called font lock.

;; Maximum colour but minimum chatter
(setq-default font-lock-maximum-decoration t
	      font-lock-verbose nil
	      font-lock-maximum-size nil)

;; Change some default  faces
(make-face-bold 'font-lock-function-name-face)

(if window-system
    (set-face-foreground 'font-lock-comment-face "FireBrick")
  ;; Consoles have less colors to play with
  (set-face-foreground 'font-lock-comment-face "red")
  (set-face-foreground 'font-lock-string-face "green")
  (set-face-foreground 'font-lock-keyword-face "blue")
  (set-face-foreground 'font-lock-variable-name-face "purple")
  )

;; -------------------------------------------------------------------------
;; font-lock-comment-warn
;; I want all comments with my initials (SAM) at the start to be very bold
(defface font-lock-comment-warn-face
  '((default (:foreground "red" :bold t :italic t)))
  "Font Lock mode face used to highlight warning comments."
  :group 'font-lock-faces)

(defconst sh-comment-warn "# ?\\<SAM\\>.*"
  "Regular expression used in scripts.")

(defun comment-warn (mode &optional re)
  "Helper for comment bolding."
  (unless re (setq re sh-comment-warn))
  (let ((keyword (list (list re 0 (quote `font-lock-comment-warn-face) t))))
    (font-lock-add-keywords mode keyword)))
