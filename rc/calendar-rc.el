;;; ----------------------------------------------
;; Calendar

(would-like 'holidays)

;; Where in the world is Ottawa?
;; google.ca/maps 45.3842398,-75.7337702
(setq calendar-location-name "Ottawa, ON"
      calendar-latitude  '[45 23 north]
      calendar-longitude '[75 44 west])

(my-feature-cond
 (holiday-christian-holidays
    (setq holiday-christian-holidays nil
	  holiday-hebrew-holidays nil
	  holiday-islamic-holidays nil
	  holiday-bahai-holidays nil
	  holiday-oriental-holidays nil
	  calendar-mark-holidays-flag t))
 (t
  (setq christian-holidays nil
	hebrew-holidays nil
	islamic-holidays nil
	bahai-holidays nil
	oriental-holidays nil
	mark-holidays-in-calendar t)))

;; Standard holidays too UScentric
(setq calendar-holidays
      '((holiday-fixed  1  1	"New Year's Day (S)")
	(holiday-fixed  2  2	"Groundhog Day")
	(holiday-fixed  2 14	"Valentine's Day")
	(holiday-float  2  1  3 "Family Day (S)")
	(holiday-fixed  3 17	"St. Patrick's Day")
	(holiday-fixed  4  1	"April Fools' Day")
	(holiday-easter-etc -2	"Good Friday (S)")
	(holiday-easter-etc 1	"Easter Monday")
	(holiday-float  5  0  2	"Mother's Day")
	(holiday-float  5  1 -1	"Victoria Day (S)" 24)
	(holiday-float  6  0  3	"Father's Day")
	(holiday-fixed  7  1	"Canada Day (S)")
	(holiday-fixed  7 16	"Slackware 1.00 1993")
	(holiday-float  8  1  1	"Civic Holiday")
	(holiday-fixed  8 16	"Debian 1993")
	(holiday-fixed  8 18	"Jeanine's Birthday")
	(holiday-float  9  1  1	"Labour Day (S)")
	(holiday-fixed  9 17	"Linux 0.01 1991")
	(holiday-float 10  1  2	"Thanksgiving (S)")
	(holiday-fixed 10 31	"Halloween")
	(holiday-fixed 11  3	"Unix V1 1971")
	(holiday-fixed 11 11	"Rememberance Day")
	(holiday-fixed 12 25	"Christmas (S)")
	(holiday-fixed 12 26	"Boxing Day (S)")
	))
