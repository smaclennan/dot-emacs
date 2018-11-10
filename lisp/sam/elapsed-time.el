(defun last-day-of-month (time)
  (nth 3 (decode-time
	  (encode-time
	   (nth 0 time) (nth 1 time) (nth 2 time) 0 (1+ (nth 4 time)) (nth 5 time)))))

(defun elapsed-time (date1 date2)
  (let ((time1 (parse-time-string date1))
	(time2 (parse-time-string date2)))
    (let ((year1 (nth 5 time1)) (mon1 (nth 4 time1)) (day1 (nth 3 time1))
	  (year2 (nth 5 time2)) (mon2 (nth 4 time2)) (day2 (nth 3 time2))
	  years months (days 0) full1 full2)

      (if (or (eq year1 year2) (eq mon1 mon2))
	  (setq years (- year2 year1))
	(setq years (- year2 year1 1)))

      (if (> mon1 mon2)
	  (setq months (+ (- 12 mon1) mon2))
	(setq months (- mon2 mon1)))

      ;; Days are hard
      (if (eq day1 1)
	  (setq full1 t))
      (if (eq (nth 3 time2) (last-day-of-month time2))
	  (setq full2 t))

      (cond
       ((eq mon1 mon2)
	(if (and full1 full2)
	    (setq months (1+ months))
	  (setq days (- day2 day1))))

	((and full1 full2)
	 (setq months (+ months 2)))

	(full1
	 (setq months (1+ months))
	 (setq days day2))

	(full2
	 (setq months (1+ months))
	 (setq days
		  (days-between
		   (format-time-string "%d %b %Y %T" (encode-time 1 0 0 0 (1+ mon1) year1))
		   (format-time-string "%d %b %Y %T" (encode-time 1 0 0 day1 mon1 year1)))))

	(t
	 (setq days
	       (+ (days-between
		   (format-time-string "%d %b %Y %T" (encode-time 1 0 0 0 (1+ mon1) year1))
		   (format-time-string "%d %b %Y %T" (encode-time 1 0 0 day1 mon1 year1)))
		  (days-between
		   (format-time-string "%d %b %Y %T" (encode-time 1 0 0 day2 mon2 year2))
		   (format-time-string "%d %b %Y %T" (encode-time 1 0 0 0 mon2 year2))))))
	)

      (list years months days))))

;;  2 10 16 (elapsed-time "27 Nov 2010 01:00:00" "13 Sep 2013 21:00:00")
;;  1  5 18 (elapsed-time "25 Apr 2011 01:00:00" "13 Sep 2013 21:00:00")
;;  1  0  2 (elapsed-time "25 Apr 2012 01:00:00" "27 Apr 2013 21:00:00")
;;  0  0  2 (elapsed-time "25 Apr 2013 01:00:00" "27 Apr 2013 21:00:00")
;; 14  5 14 (elapsed-time "1 Sep 1996 00:00:01" "14 Jan 2011 23:59:59")
;; 14  6  0 (elapsed-time "1 Sep 1996 00:00:01" "31 Jan 2011 23:59:59")
