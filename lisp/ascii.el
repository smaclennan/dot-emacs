(defconst ascii-special
  '("NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK" "BEL" "BS" "HT" "LF"
    "VT" "FF" "CR" "SO" "SI" "DLE" "DC1" "DC2" "DC3" "DC4" "NAK"
    "SYN" "ETB" "CAN" "EM" "SUB" "ESC" "FS" "GS" "RS" "US")
  "ASCII chars below space.")

(defun ascii (ascii)
  "Show ASCII number.
Supports C style numbers or Emacs style numbers:
  0141 or #o141 is octal
  0x61 or #x61  is hex
  #b1100001     is binary
  97            is decimal
  ?a            is char
The previous will all show:
  141 97 61 \\='a\\='"
  (interactive
   (list
    (let ((nstr (read-string "ascii: ")))
      (cond
       ((string-match "^\\(0\\|#o]\\)[0-7]*$" nstr)
	(string-to-number nstr 8)) ;; octal
       ((string-match "^\\(0x\\|#x\\)\\([0-9a-fA-F]+\\)$" nstr)
	(string-to-number (match-string 2 nstr) 16)) ;; hex
       ((string-match "^[0-9]+$" nstr)
	(string-to-number nstr 10)) ;; decimal
       ((string-match "^#b\\([0-1]+\\)$" nstr)
	(string-to-number (match-string 1 nstr) 2)) ;; binary
       ((string-match "^?.$" nstr) ;; char
	(string-to-char (substring nstr 1)))
       (t (error "Invalid input %s" nstr))))))
  (let (outstr)
    (cond
     ((and (>= ascii ? ) (<= ascii ?~)) (setq outstr (format "'%c'" ascii)))
     ((< ascii ? ) (setq outstr (nth ascii ascii-special)))
     ((eq ascii 127) (setq outstr "DEL"))
     (t (setq outstr "Non-ASCII")))
    (message "%o %d %x %s" ascii ascii ascii outstr)))
