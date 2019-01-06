;; Some handy functions for dealing with IPv4

(defun ip-to-number (ip)
  "Convert an IP string to a number. Warning: Assumes little endian."
  (unless (string-match (concat "\\([0-9]\\{1,3\\}\\)\\."
				"\\([0-9]\\{1,3\\}\\)\\."
				"\\([0-9]\\{1,3\\}\\)\\."
				"\\([0-9]\\{1,3\\}\\)") ip)
    (error "Malformed ip %s" ip))
  (let ((n1 (string-to-number (match-string 1 ip)))
	(n2 (string-to-number (match-string 2 ip)))
	(n3 (string-to-number (match-string 3 ip)))
	(n4 (string-to-number (match-string 4 ip))))
    (when (or (> n1 255) (> n2 255) (> n3 255) (> n4 255))
      (error "Malformed ip %s" ip))
    (+ n4 (lsh n3 8) (lsh n2 16) (lsh n1 24))))

(defun bits-to-mask (bits)
  "Given BITS return the mask. BITS can be a number or a
string. The mask is returned as a number."
  (when (stringp bits) (setq bits (string-to-number bits)))
  (logand #xffffffff (lognot (- (lsh 1 (- 32 bits)) 1))))

(defun bits-to-maskstr (bits)
  "Given BITS as a number or a string return the a mask
string. e.g. 23 = 255.255.254.0"
  (let ((num (bits-to-mask bits)) str)
    (setq str (number-to-string (logand num #xff)))
    (loop for i from 1 to 3 do
	  (setq num (lsh num -8))
	  (setq str (concat (number-to-string (logand num #xff)) "." str)))
    str))
