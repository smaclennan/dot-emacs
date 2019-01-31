;;; ----------------------------------------------
;; Calendar

(require 'holidays)

;; Where in the world is Ottawa?
;; google.ca/maps 45.3842398,-75.7337702
(setq calendar-location-name "Ottawa, ON"
      calendar-latitude  '[45 23 north]
      calendar-longitude '[75 44 west])

(setq calendar-mark-holidays-flag t)

(defun corrected-dow (date)
  "Given a list of month, day, year return the week day. Sunday = 7."
  (let ((dow (calendar-day-of-week date)))
    (if (eq dow 0) 7 dow)))

;; Standard holidays too UScentric
(setq calendar-holidays
      (append
       '((holiday-fixed  2  2	  "Groundhog Day")
	 (holiday-fixed  2 14	  "Valentine's Day")
	 (holiday-float  2  1  3  "Family Day (S)")
	 (holiday-fixed  3 17	  "St. Patrick's Day")
	 (holiday-fixed  4  1	  "April Fools' Day")
	 (holiday-easter-etc -2	  "Good Friday (S)")
	 (holiday-easter-etc 1	  "Easter Monday")
	 (holiday-float  5  0  2  "Mother's Day")
	 (holiday-float  5  1 -1  "Victoria Day (S)" 24)
	 (holiday-float  6  0  3  "Father's Day")
	 (holiday-fixed  7  1	  "Canada Day (S)")
	 (holiday-fixed  7 16	  "Slackware 1.00 1993")
	 (holiday-float  8  1  1  "Civic Holiday")
	 (holiday-fixed  8 16	  "Debian 1993")
	 (holiday-fixed  8 18	  "Jeanine's Birthday")
	 (holiday-float  9  1  1  "Labour Day (S)")
	 (holiday-fixed  9 17	  "Linux 0.01 1991")
	 (holiday-float 10  1  2  "Thanksgiving (S)")
	 (holiday-fixed 10 31	  "Halloween")
	 (holiday-fixed 11  3	  "Unix V1 1971")
	 (holiday-fixed 11 11	  "Rememberance Day")
	 ;; Christmas/Boxing Day/New Year's
	 ;; 2009 - 2011 good years to check
	 (holiday-sexp '(if (> (corrected-dow (list 12 25 year)) 5)
			    (list 12 25 year)) "Christmas")
	 (holiday-sexp '(if (memq (corrected-dow (list 12 26 year)) '(6 7 1))
			    (list 12 26 year)) "Boxing Day")
	 (holiday-sexp '(if (> (corrected-dow (list 1 1 year)) 5)
			    (list 1 1 year))   "New Year's Day")
	 (holiday-sexp '(let ((dow (corrected-dow (list 12 25 year))))
			  (if (eq dow 7) ;; Sunday
			      (list 12 26 year)
			    (if (eq dow 6) ;; Saturday
				(list 12 27 year)
			      (list 12 25 year))))
		       "Christmas (S)")
	 (holiday-sexp '(let ((dow (corrected-dow (list 12 26 year))))
			  (if (> dow 5) ;; Sunday/Saturday
			      (list 12 28 year)
			    (if (eq dow 1) ;; Monday
				(list 12 27 year)
			      (list 12 26 year))))
		       "Boxing Day (S)")
	 (holiday-sexp '(let ((dow (corrected-dow (list 1 1 year))))
			  (if (eq dow 7) ;; Sunday
			      (list 1 2 year)
			    (if (eq dow 6) ;; Saturday
				(list 1 3 year)
			      (list 1 1 year))))
		       "New Year's Day (S)"))
       ;; Allow local holidays
       holiday-local-holidays))

;; -------------------------
;; Diary is part of calendar

;; Handy function for those who work Monday to Friday
(defun diary-workday (&optional time)
  (memq (nth 6 (decode-time time)) '(1 2 3 4 5)))

(setq diary-comment-start "#")

(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
