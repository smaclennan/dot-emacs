(defvar bisect-good nil
  "Known good number")

(defvar bisect-bad nil
  "Known bad number")

(defvar bisect-cur nil
  "Current bisect")

(defvar bisect-save-file "~/.bisect-save"
  "File to save bisect state in.
Warning: This file is deleted on a `bisect-end'.")

(defun bisect-start (good bad)
  (interactive "nGood: \nnBad: ")
  (and bisect-good bisect-bad
       (eq (yes-or-no-p "Overwrite current good/bad? ") nil)
       (error "Abort"))
  (setq bisect-good good
	bisect-bad  bad
	bisect-cur  (bisect good bad))
  (bisect-save)
  (message "Next %d" bisect-cur))

(defun bisect-end ()
  (interactive)
  (message "Current %d" bisect-cur)
  (setq bisect-good nil bisect-bad nil)
  (delete-file bisect-save-file))

(defun bisect (good bad)
  "Bisect good and bad with no side-effects."
  (when (> good bad) (cl-rotatef good bad))
  (+ (/ (- bad good) 2) good))

(defun bisect-good ()
  (interactive)
  (setq bisect-good bisect-cur)
  (setq bisect-cur (bisect bisect-good bisect-bad))
  (bisect-save)
  (message "Next %d" bisect-cur))

(defun bisect-bad ()
  (interactive)
  (setq bisect-bad bisect-cur)
  (setq bisect-cur (bisect bisect-good bisect-bad))
  (bisect-save)
  (message "Next %d" bisect-cur))

(defun bisect-save ()
  (when bisect-save-file
    (write-region
     (format "G %d C %d B %d\n"
	     bisect-good bisect-cur bisect-bad)
     nil bisect-save-file)))

(defun bisect-restore ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents bisect-save-file)
    (re-search-forward "^G \\([0-9]+\\) C \\[0-9]+\\) B \\([0-9]+\\)$")
    (setq bisect-good (string-to-number (match-string 1))
	  bisect-cur  (string-to-number (match-string 2))
	  bisect-bad  (string-to-number (match-string 3)))))
