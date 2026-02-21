;; -*- lexical-binding: t; -*-

;; Trying to keep track of how much water (or equivalent) I drink
;; 3.7L for men and 2.7L for women is considered good

(defvar drinks-size-list '(("355ml can" 355)
			   ("qnx mug" 300)
			   ("brita" 630)
			   ("tea pot" 700)
			   ("coffee" 800)
			   ("glass" 250)
			   ("ensure" 235)
			   ("other" 0)))

;; Assumes a single instance accesses the file
(defvar drinks-file "~/.drinks-file")
(defvar drinks-list nil)

(defun drinks-init ()
  (when (null drinks-list)
    (load drinks-file t t t)))

;;;###autoload
(defun drinks-add (drink)
  (interactive
   (list
    (completing-read (concat "Drink [" (caar drinks-size-list) "]: ")
		     drinks-size-list nil t nil nil (caar drinks-size-list))))
  (when (string= drink "other")
    (let ((size (read-number "Size (ml): ")))
      (setcdr (assoc "other" drinks-size-list) (list size))))
  
  (drinks-init)
  
  (let* ((today (format-time-string "%Y/%m/%d"))
	 (entry (assoc today drinks-list))
	 (size (cadr (assoc drink drinks-size-list))))
    (if entry
	(setcdr entry (list (+ (cadr entry) size)))
      ;; create new entry
      (setq drinks-list (append drinks-list (list (list today size))))))

  ;; Update the file 
  (with-temp-file drinks-file
    (insert "(setq drinks-list '" (prin1-to-string drinks-list) ")\n"))

  (drinks-show))
(global-set-key "\C-ca" 'drinks-add)

;;;###autoload
(defun drinks-show ()
  (interactive)
  (drinks-init)
  (let* ((date (format-time-string "%Y/%m/%d"))
	 (today (assoc date drinks-list)))
    (if today
	(message "%s %.1fL" (car today) (/ (float (cadr today)) 1000))
      (message "%s empty" date))))
