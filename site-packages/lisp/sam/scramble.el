;; My wife likes to do the "ryhmes with bumble" in the newspaper.
;; This helps to unscrable the words she can't get.
;; It works great up to 8 chars... which is more than enough.
;; 9 chars = ~5s, 10 chars = ~45s, 11 chars = ~10m.
;; 12 chars needs too much memory/disk space.
;; Number of combinations = n factorial.

(defun unscramble-level (word wlist new)
    (if wlist
	(dolist (a wlist)
	  (unscramble-level word
			    (remove a wlist)
			    (concat new (string (aref word a)))))
      (insert new "\n")))

;;;###autoload
(defun unscramble (word)
  "Unscramble a scrambled word. May display multiple words."
  (interactive "sWord: ")
  (let* ((len (1- (length word)))
	 (wlist (number-sequence 0 len))
	 (fname (make-temp-file "unscram"))
	 (buff (find-file-noselect fname))
	 str)
    (save-current-buffer
      (set-buffer buff)
      (buffer-disable-undo)
      (unscramble-level word wlist nil)
      (save-buffer)
      ;; We can get duplicate words if the input contains duplicate letters
      (setq str (shell-command-to-string (concat "hunspell -G " fname " | sort -u"))))
    (kill-buffer buff)
    (delete-file fname)
    (setq str (replace-regexp-in-string "\n" " " str))
    (message "%s" (replace-regexp-in-string " $" "" str))
    ))
