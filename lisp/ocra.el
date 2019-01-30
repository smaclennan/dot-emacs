;; ocra.el - Very minimal RFC6287 HOTP
;; Copyright (C) 2013 Sean MacLennan <seanm@seanm.ca>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this project; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'hmac)
(require 'sam-common)

(defconst ocra-question-len 128)

(defvar ocra-64bit (or (not (eq most-positive-fixnum #x3fffffff))
		       (functionp 'bignump))
  "Non-nil if we can support 31-bit numbers.
Normal 32-bit emacs can only support 30-bit numbers.")

(eval-when-compile
  (defmacro ocra-hex-char-to-num (chr)
    `(let ((chr ,chr))
       (cond
	((and (<= ?a chr)(<= chr ?f)) (+ (- chr ?a) 10))
	((and (<= ?A chr)(<= chr ?F)) (+ (- chr ?A) 10))
	((and (<= ?0 chr)(<= chr ?9)) (- chr ?0))
	(t (error "Invalid hexadecimal digit `%c'" chr))))))

;; This and hex-char-to-num are from hex-utils.el. I just could not
;; get it to compile cleanly in emacs.
(defun ocra-decode-hex-string (string)
  "Decode hexadecimal STRING to octet string."
  (let* ((len (length string))
	 (dst (make-string (/ len 2) 0))
	 (idx 0)(pos 0))
    (while (< pos len)
      ;; logior and lsh are not byte-coded.
      ;; (aset dst idx (logior (lsh (hex-char-to-num (aref string pos)) 4)
      ;; 			    (hex-char-to-num (aref string (1+ pos)))))
      (aset dst idx (+ (* (ocra-hex-char-to-num (aref string pos)) 16)
		       (ocra-hex-char-to-num (aref string (1+ pos)))))
      (setq idx (1+ idx)
	    pos (+ 2 pos)))
    dst))

(defun ocra-sha1-binary (string) (sha1 string nil nil t))

(defconst sha-digest-length 20)

(defun ocra-mangle (question)
  (let (len tmp odd)

    (setq tmp (make-string ocra-question-len 0))

    ;; count nibbles
    (cl-loop for i from 0 to 7 with mask = 0 while (> question mask) finally (setq len i) do
      (setq mask (+ (lsh mask 4) #xf)))

    (setq odd (= (logand len 1) 1))
    (setq len (/ len 2)) ; convert from nibbles to bytes

    (when odd
      (aset tmp len(lsh (logand question #xf) 4))
      (setq question (lsh question -4))
      )

    (setq len (1- len))

    (cl-loop for i from len downto 0 do
      (aset tmp i (logand question #xff))
      (setq question (lsh question -8)))

    tmp))

;; RFC6287
;;;###autoload
(defun ocra-generate (question secret &optional suite)
  (let ((power 100000000) ;; default 8
	msg hash offset code-digits)

    (unless suite (setq suite "OCRA-1:HOTP-SHA1-8:QN08"))
    (setq secret (ocra-decode-hex-string secret))

    (when (string-match "OCRA-1:HOTP-SHA1-\\([1-8]\\):" suite)
      (setq code-digits (string-to-number (match-string 1 suite)))
      (setq power (expt 10 code-digits)))

    (setq msg (concat suite (make-string 1 0) (ocra-mangle question)))
    (setq hash (hmac secret msg 'ocra-sha1-binary sha-digest-length))
    (setq offset (logand (aref hash (- sha-digest-length 1)) #xf))

    (if (or ocra-64bit (>= #x3f (aref hash offset)))
	;; 64 bit just works. We are also good if high byte is in range for 32-bit.
	(mod (logior (lsh (logand (aref hash offset) #x7f) 24)
		     (lsh (logand (aref hash (+ offset 1)) #xff) 16)
		     (lsh (logand (aref hash (+ offset 2)) #xff) 8)
		     (logand (aref hash (+ offset 3)) #xff))
	     power)
      ;; 32 bit emacs cannot handle 31 bit numbers. Since I do not use
      ;; this in a time critical manner (and usually 64bit anyway),
      ;; lets just let dc do the work!
      (let ((out (format "echo -e '16i\\n%02X%02X%02X%02X\\n%X\\n%%p' | dc"
			 (logand (aref hash offset) #x7f)
			 (logand (aref hash (+ offset 1)) #xff)
			 (logand (aref hash (+ offset 2)) #xff)
			 (logand (aref hash (+ offset 3)) #xff)
			 power)))
	(string-to-number (shell-command-to-string out))))
    ))

;;;###autoload
(defun ocra-generate-from-file (question file &optional suite)
  (let ((secret (with-temp-buffer
		  (insert-file-contents file)
		  (buffer-string))))
    (ocra-generate question secret suite)))

(defun ocra-test-vectors ()
  (let (q r e failed
	(ocra-test-key "3132333435363738393031323334353637383930"))

    (dolist
	(test '(;; Standard test vectors from RFC6287
		(00000000 . 237653)
		(11111111 . 243178)
		(22222222 . 653583)
		(33333333 . 740991)
		(44444444 . 608993)
		(55555555 . 388898)
		(66666666 . 816933)
		(77777777 . 224598)
		(88888888 . 750600)
		(99999999 . 294470)
		;; Some shorter tests to check mangle
		(11 . 807261)
		(111 . 901657)
		(1111 . 134216)
		(11111 . 861721)
		(111111 . 275430)
		(1111111 . 960547)
		))
      (setq q (car test))
      (setq r (ocra-generate q ocra-test-key "OCRA-1:HOTP-SHA1-6:QN08"))
      (setq e (cdr test))
      (unless (eq r e)
	(setq failed t)
	(message "%08u got %d/%x expected %d/%x" q r r e e))
    )

    (if failed
	(message "FAILED")
      (message "Success!"))
    ))

;(ocra-test-vectors)

(provide 'ocra)
