;; -*- lexical-binding: t -*-

(defvar filter-f nil)
(defvar filter-c nil)
(defvar filter-r nil)

(defconst filter-pi 3.14159)

(defun filter-calc (a b)
  (/ 1.0 (* a b 2 filter-pi)))

;;;###autoload
(defun filter ()
  (interactive)
  (let (f c r)
    (setq filter-c (read-from-minibuffer "C (uF): " filter-c))

    ;; string-to-number dosen't like 0.1u
    (if (string-match "\\([0-9.]+\\) *\\([a-zA-Z]\\)" filter-c)
	(let ((unit (downcase (string-to-char (match-string 2 filter-c)))))
	  (setq c (string-to-number (match-string 1 filter-c)))
	  (cond
	   ((eq unit ?p) (setq c (/ c 1000000.0)))
	   ((eq unit ?n) (setq c (/ c 1000.0)))
	   ((eq unit ?u) t) ;; nop
	   ((eq unit ?f) (setq c (* c 1000000.0)))
	   (t (error "Unknown unit %S" unit))))
      (setq c (string-to-number filter-c)))

    (setq filter-r (read-from-minibuffer "R (ohms): " filter-r))
    ;; Convert r to M
    (if (string-match "\\([0-9.]+\\) *\\([a-zA-Z]\\)" filter-r)
	(let ((unit (downcase (string-to-char (match-string 2 filter-r)))))
	  (setq r (string-to-number (match-string 1 filter-r)))
	  (cond
	   ((eq unit ?k) (setq r (/ r 1000.0)))
	   ((eq unit ?m) t) ;; nop
	    (t (error "Unknown unit %S" unit))))
      (setq r (string-to-number filter-r))
      (if (not (eq r 0))
	  (setq r (/ r 1000000.0))))

    (if (or (eq c 0) (eq r 0))
	;; f needed
	(progn
	  (setq filter-f (read-from-minibuffer "F (hz): " filter-f))
	  (setq f (string-to-number filter-f))

	  (when (string-match "[0-9.]+ *\\([a-zA-Z]\\)" filter-f)
	    (let ((unit (downcase (string-to-char (match-string 1 filter-f)))))
	      (cond
	       ((eq unit ?k) (setq f (* f 1000.0)))
	       ((eq unit ?m) (setq f (* f 1000000.0)))
	       (t (error "Unknown unit %S" unit)))))


	  (if (eq c 0)
	      ;; calc c
	      (setq c (filter-calc f r))
	    (setq r (filter-calc f c))))
      ;; f calc
      (setq f (filter-calc c r)))

    (setq r (* r 1000.0))

    (let ((c-units "uF") (r-units "k") (f-units "Hz"))
      (when (< c 0.001)
	(setq c-units "pF")
	(setq c (* c 1000000.0)))
      (when (> f 1049.0)
	(setq f-units "kHz")
	(setq f (/ f 1000.0)))
      (message "f: %.3f %s  c: %.3f %s  r: %.1f %s" f f-units c c-units r r-units))))
