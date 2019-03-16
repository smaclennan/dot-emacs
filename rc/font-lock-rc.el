;; From the XEmacs font-lock.el:
;; It's called font-lock-mode here because on the Lispms it was called
;; "Electric Font Lock Mode."  It was called that because there was an older
;; mode called "Electric Caps Lock Mode" which had the function of causing all
;; of your source code to be in upper case except for strings and comments,
;; without you having to blip the caps lock key by hand all the time (thus the
;; "electric", as in `electric-c-brace'.)

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
