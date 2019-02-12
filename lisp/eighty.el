;; Hightlight lines >= 80 columns. This is not dynamic, you must keep
;; calling `80-scan' after edits.

(defvar 80-list nil "Internal list of lines >= 80 columns.")

(defvar 80-state nil "Internal state of 80.")

(defgroup 80-highlighting nil
  "Face for highlighting."
  :prefix "80-"
  :group '80-group)

(defface 80-highlight
  '((((class color))  (:background "yellow"))
    (t (:underline t)))
  "Face for highlighting."
  :group '80-highlighting)

(defun 80-mark-line ()
  "Mark the part of a line >= 80 columns."
  (let* ((col (current-column))
	 (to (point))
	 (from (- to col -80))
	 (extent (make-overlay from to)))
    (overlay-put extent 'face '80-highlight)
    (overlay-put extent 'tag 80)
    (setq 80-list (nconc 80-list (list extent)))))

;;;###autoload
(defun 80-scan ()
  "Scan buffer for lines >= 80 columns."
  (interactive)
  (80-cleanup)
  (save-excursion
    (let ((clean t))
      (goto-char (point-min))
      (end-of-line)
      (while (not (eobp))
	(when (>= (current-column) 80)
	  (80-mark-line)
	  (setq clean nil))
	(end-of-line 2))
      (message (if clean "Clean" "Dirty"))
      )))

;;;###autoload
(defun 80-cleanup ()
  "Remove all the 80 column highlighting."
  (interactive)
  (dolist (extent 80-list)
    (delete-overlay extent))
  (setq 80-list nil))

;;;###autoload
(defun 80-toggle ()
  "Toggle between 80-scan and 80-cleanup."
  (interactive)
  (if 80-state
      (progn
	(80-cleanup)
	(setq 80-state nil)
	(message "80 disabled"))
    (80-scan)
    (setq 80-state t)
    (message "80 enabled")))

;;;###autoload
(defun 80-goto-next ()
  "Goto the next line >= 80 columns. You do not have to scan first."
  (interactive)
  (end-of-line 2)
  (while (not (or (eobp) (>= (current-column) 80)))
    (end-of-line 2)))
