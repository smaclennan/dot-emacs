;; Some handy functions for dealing with IPv4

(require 'cl-macs)

(defun ip-to-number (ip)
  "Convert an IP string to a number. Warning: Assumes little endian."
  (unless (string-match
	   "\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)"
	   ip)
    (error "Malformed ip %s" ip))
  (let ((n 0) n1)
    (dolist (i '(1 2 3 4))
      (setq n1 (string-to-number (match-string i ip)))
      (if (> n1 255) (error "Malformed ip %s" ip))
      (setq n (logior (lsh n 8) n1)))
    n))

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
    (cl-loop for i from 1 to 3 do
	  (setq num (lsh num -8))
	  (setq str (concat (number-to-string (logand num #xff)) "." str)))
    str))
