;; -*- lexical-binding: t -*-
;; Use:
;; 1) `bisect-start' or `bisect-start-list'
;; 2) `bisect-good' or `bisect-bad' as the case may be
;; 3) `bisect-end' when done
;;
;; By default the bisect information is stored in
;; `bisect-save-file'. This allows you to bisect over reboots or
;; restarts of Emacs. Use `bisect-restore' to recover the info.

(defvar bisect-list nil
  "List of numbers to bisect. Use `bisect-start-list'.
This is useful for bisecting an object in subversion where the
changes are far apart.")

(defvar bisect-good nil
  "Known good number")

(defvar bisect-bad nil
  "Known bad number")

(defvar bisect-cur nil
  "Current bisect")

(defvar bisect-save-file "~/.bisect-save"
  "File to save bisect state in.
Warning: This file is deleted on a `bisect-end'.")

(require 'cl-macs)

(defun bisect-cur ()
  (if bisect-list
      (nth bisect-cur bisect-list)
    bisect-cur))

;;;###autoload
(defun bisect-start (good bad)
  (interactive "nGood: \nnBad: ")
  (and bisect-good bisect-bad
       (eq (yes-or-no-p "Overwrite current good/bad? ") nil)
       (error "Abort"))
  (when (called-interactively-p 'interactive)
    (setq bisect-list nil))
  (setq bisect-good good
	bisect-bad  bad
	bisect-cur  (bisect good bad))
  (bisect-save)
  (message "Next %d" (bisect-cur)))

;;;###autoload
(defun bisect-start-list ()
  "It is assumed the first element is bad and the last element is good."
  (interactive)
  (unless bisect-list (error "The list is empty"))
  (bisect-start (1- (length bisect-list)) 0))

;;;###autoload
(defun bisect-end ()
  (interactive)
  (message "Current %d" (bisect-cur))
  (setq bisect-good nil bisect-bad nil bisect-list nil)
  (delete-file bisect-save-file))

(defun bisect (good bad)
  "Bisect good and bad with no side-effects."
  (when (> good bad) (cl-rotatef good bad))
  (+ (/ (- bad good) 2) good))

(defun bisect-next ()
  (setq bisect-cur (bisect bisect-good bisect-bad))
  (bisect-save)
  (if (eq bisect-cur bisect-bad)
      (message "BAD %d" (bisect-cur))
    (message "Next %d" (bisect-cur))))

;;;###autoload
(defun bisect-good ()
  (interactive)
  (setq bisect-good bisect-cur)
  (bisect-next))

;;;###autoload
(defun bisect-bad ()
  (interactive)
  (setq bisect-bad bisect-cur)
  (bisect-next))

(defun bisect-save ()
  (when bisect-save-file
    (write-region
     (format "G %d C %d B %d\n%S"
	     bisect-good bisect-cur bisect-bad bisect-list)
     nil bisect-save-file)))

;;;###autoload
(defun bisect-restore ()
  (interactive)
  (with-temp-buffer
    (insert-file-contents bisect-save-file)
    (re-search-forward "^G \\([0-9]+\\) C \\([0-9]+\\) B \\([0-9]+\\)$")
    (setq bisect-good (string-to-number (match-string 1))
	  bisect-cur  (string-to-number (match-string 2))
	  bisect-bad  (string-to-number (match-string 3)))
    (when (re-search-forward "^(\\([0-9 ]+\\))" nil t)
      (setq bisect-list
	    (mapcar 'string-to-number (split-string (match-string 1)))))))
