(require 'sam-common) ;; my-interactive-p

(defun inches-print (list)
  "Pretty print the `inches-decimal-to-fraction' output."
  (let ((feet (nth 0 list)) (inches (nth 1 list))
	(frac (nth 2 list)) (denom (nth 3 list))
	str ftstr space)
    (if (> feet 0)
	(setq ftstr (format "%d' %d" feet inches)
	      str (format "%d" (+ (* feet 12) inches))
	      space " ")
      (if (> inches 0)
	  (setq str (format "%d" inches)
		space " ")))
    (when (> frac 0)
      (setq str (concat str space (format "%d/%d" frac denom)))
      (when ftstr (setq ftstr (format "%s %d/%d" ftstr frac denom))))
    (if ftstr
	(message "%s\"  or  %s\"" str ftstr)
      (message "%s\"" str))))

;;;###autoload
(defun inches-decimal-to-fraction (denom inches &optional fixed)
  "Convert decimal inches to a fraction.

If DENOM is nil, defaults to 16ths. Interactively a
universal-prefix sets the DENOM.
If FIXED is non-nil, does not reduce the fraction.

When non-interactive returns a list (feet inches fraction denom)."
  (interactive "P\nnInches: ")
  (unless denom (setq denom 16))
  (let ((feet (floor (/ inches 12)))
	(frac (round (* (mod inches 1) denom)))
	out)
    (when (> feet 0)
      (setq inches (- inches (* feet 12))))
    (setq inches (floor inches))
    (unless fixed
      (if (> frac 0)
	  (while (eq (logand frac 1) 0)
	    (setq frac (/ frac 2))
	    (setq denom (/ denom 2)))
	(setq denom 0)))
    (setq out (list feet inches frac denom))
    (when (my-interactive-p) (inches-print out))
    out))

;;;###autoload
(defun mm2inches (denom mm)
  "Print millimeters as inches. See `inches-decimal-to-fraction'.

If DENOM is 1000, show as inch decimal in thousandths."
  (interactive "P\nnmm: ")
  (let ((inch (* mm 0.0393701)))
    (if (eq denom 1000)
	(message "%.3f" inch)
      (inches-print (inches-decimal-to-fraction denom inch)))))

;;;###autoload
(defun mm2inches-64ths (denom mm)
  "Print millimeters as 64ths of an inch."
  (interactive "P\nnmm: ")
  (inches-print (inches-decimal-to-fraction 64 (* mm 0.0393701) t)))

;;;###autoload
(defun cm2inches (denom cm)
  "Print centimeters as feet/inches. See `inches-decimal-to-fraction'."
  (interactive "P\nncm: ")
  (mm2inches denom (* cm 10)))

;;;###autoload
(defun imperial2cm (feet inches)
  "Handy for converting feet + inches to cms for heights."
  (interactive "nfeet: \nninches: ")
  (let ((cm (* (+ (* feet 12) inches) 2.54)))
    (message "%dcm" cm)))

;;;###autoload
(defun m2ft (meters)
  "Convert METERS to feet. Handy for heights like 1.68m."
  (interactive "nMeters: ")
  (let* ((feet (* meters 3.281))
	 (inches (* 12 (mod feet 1))))
    (message "%d' %d\"" feet inches)))

;;;###autoload
(defun m2inches (denom m)
  "Print meters as feet/inches. See `inches-decimal-to-fraction'."
  (interactive "P\nnm: ")
  (mm2inches denom (* m 1000)))

;;;###autoload
(defun kg2lbs (kg)
  "Convert KG to pounds and ounces."
  (interactive "nkg: ")
  (let* ((raw (* kg 2.20462))
	 (lbs (floor raw))
	 (oz (round (* 16 (- raw lbs)))))
    (message "%d lbs %d oz" lbs oz)
    (list lbs oz)))

;;;###autoload
(defun lbs2kg (lbs &optional oz)
  "Convert LBS to kgs. Interactively, if you give two numbers it is lbs and oz."
  (interactive "slbs: ")
  (let (result kg)
    (if (stringp lbs)
	(progn
	  (setq result (string-to-number lbs))
	  (when (string-match "[0-9]+[[:blank:]]+\\([0-9]+\\)" lbs)
	    (setq result (+ result (/ (float (string-to-number (match-string 1 lbs))) 16)))))
      (unless oz (setq oz 0))
      (setq result (+ lbs (/ (float oz) 16))))
    (setq kg (/ result 2.20462))
    (message "%.2f kg" kg)
    kg))

;;;###autoload
(defun l2ci (liters)
  "Convert LITERS to cubic inches."
  (interactive "nL: ")
  (let ((ci (round (* liters 61.024))))
    (message "%SL = %Sci" liters ci)
    ci))
