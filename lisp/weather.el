;; -*- lexical-binding: t -*-

(defvar weather-url "https://weather.gc.ca/city/pages/on-118_metric_e.html"
  "The url for the weather. Must be weather.gc.ca.")

(defvar weather-fancy t
  "Display a fancy version of the weather in a buffer.
Only works for X11.")

;;;###autoload
(defun weather ()
  (interactive)
  (let (gif tempc tempf conditions giffile)
    (with-current-buffer (url-retrieve-synchronously weather-url t)
      (goto-char (point-min))
      (re-search-forward "src=\"/\\(weathericons/[0-9]+\\.gif\\)\" alt=\"\\([^\"]+\\)")
      (setq gif (match-string 1))
      (setq conditions (match-string 2))
      (re-search-forward "mrgn-bttm-sm lead mrgn-tp-sm")
      (re-search-forward "<span data-v-[0-9a-f]+>\\([0-9]+\\)")
      (setq tempc (match-string 1)))

    (setq tempf (+ (/ (* (string-to-number tempc) 9) 5) 32))
    (setq tempf (number-to-string tempf))

    (if (and weather-fancy (eq window-system 'x))
	(progn
	  ;; get the gif if necessary
	  (setq giffile (concat "~/." gif))
	  (unless (file-exists-p giffile)
	    (let ((gifurl (concat "https://weather.gc.ca/" gif)))
	      (with-current-buffer (url-retrieve-synchronously gifurl t)
		(goto-char (point-min))
		(let ((case-fold-search nil)) (search-forward "GIF"))
		(delete-region (point-min) (match-beginning 0))
		(write-file giffile))))

	  (with-current-buffer (get-buffer-create "*weather*")
	    (erase-buffer)
	    (insert-image (create-image giffile))
	    (insert " " conditions " " tempc "\u00b0C " tempf "\u00b0F\n"))

	  (display-buffer-at-bottom (get-buffer "*weather*") '((window-height . 6))))
	(message "%s  %sC  %sF" conditions tempc tempf))))
