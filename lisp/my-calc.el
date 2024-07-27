;; -*- lexical-binding: t -*-
;;; my-calc.el --- A simple calculator

;; Copyright (C) 1997-2016 Sean MacLennan
;; Revision:   1.8

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This package implements a simple calculator. It understands most of
;; the 'C' operators and the order of operation. It also does hex,
;; octal, and decimal conversions.

;; The calculator is based on the operator-precedence parsing algorithm
;; from "Compilers Principles, Techniques, and Tools"
;; by Alfred V. Aho, Ravi Sethi, and Jeffery D. Ullman.

(require 'stack)
(provide 'my-calc)

(defconst my-calc-f-values
  '((?*  . 12) (?/  . 12) (?% . 12)
    (?+  . 10) (?-  . 10)
    (?<  .  8) (?>  .  8)
    (?&  .  6) (?^  .  4) (?| .  2)
    (?\( .  0) (?\) . 14)
    (?N  . 14)			; number
    (?=  .  0))			; terminator
  "Values for the precedence function `f'.")

(defconst my-calc-g-values
  '((?*  . 11) (?/  . 11) (?% . 11)
    (?+  .  9) (?-  .  9)
    (?<  .  7) (?>  .  7)
    (?&  .  5) (?^  .  3) (?| .  1)
    (?\( . 13) (?\) .  0)
    (?N  . 13)			; number
    (?=  .  0))			; terminator
  "Values for the precedence function `g'.")

(defconst my-calc-commands
  '((?* . *) (?/ . /) (?% . mod)
    (?+ . +) (?- . -)
    (?< . ash) (?> . rash)
    (?& . logand) (?^ . logxor) (?| . logior))
  "Command char to lisp command")

(defvar calc-nums (stack-create)
  "Internal stack of numbers")

(defun rash (a b)
  (ash a (- 0 b)))

(defun my-calc-f (cmd)
  "Precedence function `f'."
  (let ((lookup (cdr (assoc cmd my-calc-f-values))))
    (if lookup
	lookup
      (error "Syntax error"))))

(defun my-calc-g (cmd &optional number)
  "Precedence function `g'."
  ;; Only the `g' function is ever looking at a number.
  ;; It reads the number, pushes it on the calc-nums stack,
  ;; and replaces the number with the token `N' in the buffer.
  (if (and number (setq number (my-calc-get-number)))
      (progn
	;; Push on calc-nums stack
	(stack-push calc-nums number)
	;; Replace number with token
	;;(replace-match "N")
	(forward-char -1)
	;; Lookup N below.
	(setq cmd ?N)))

  (let ((lookup (cdr (assoc cmd my-calc-g-values))))
    (if lookup
	lookup
      (error "Syntax error"))))

(defun my-calc-get-number ()
  "Read the next number in the buffer and returns the number or nil.
Handles octal/hex/float/integer. Understands the suffixes: k or K, m
or M, g or G, and p or P. The p suffix is for 4k pages."
  (let (n)
    (cond
     ;; Octal?
     ((looking-at "\\(0[0-7]+\\)")
      (setq n (string-to-number (match-string 1) 8)))
     ;; Hex?
     ((looking-at "\\(0[xX]\\)\\([0-9a-fA-F]+\\)")
      (setq n (string-to-number (match-string 2) 16)))
     ;; Integer/float?
     ((looking-at "\\([0-9]*\\.?[0-9]+\\)")
      (setq n (string-to-number (match-string 1))))
     ;; Negative integer/float?
     ;; The preceding token must be an op (not a number)
     ((and (looking-at "\\(-[0-9]*\\.?[0-9]+\\)")
	   (or (not (eq (preceding-char) ?N)) (bobp)))
      (setq n (string-to-number (match-string 1)))))
    (when n
      (replace-match "N")
      (cond
       ((looking-at "[kK]") (setq n (ash n 10)) (replace-match ""))
       ((looking-at "[mM]") (setq n (ash n 20)) (replace-match ""))
       ((looking-at "[gG]") (setq n (ash n 30)) (replace-match ""))
       ((looking-at "[tT]") (setq n (ash n 40)) (replace-match ""))
       ((looking-at "[pP]") (setq n (ash n 12)) (replace-match ""))))
    n))

(defun my-calc-comma (n)
  (let (s)
    (while (>= (abs n) 1000)
      (setq s (concat (format ",%03d" (mod (abs n) 1000)) s))
      (setq n (/ n 1000)))
    (setq s (concat (number-to-string n) s))
    s))

(defun my-calc-float-comma (f)
  (let ((in (format "%f" f)) s)
    (when (string-match "^-?[0-9]+" in)
      (setq s (my-calc-comma (string-to-number (match-string 0 in)))))
    (when (string-match "\\.[0-9]+" in)
      (unless (string-match "\\.0+$" in)
	(setq s (concat s (match-string 0 in)))))
    s))

(defun my-calc-hex (x)
  (let ((in (format "%x" x)) out need-space)
    (while (string-match "[0-9a-fA-F]\\{1,4\\}$" in)
      (if need-space
	  (setq out (concat (match-string 0 in) " " out))
	(setq out (concat (match-string 0 in) out)))
      (setq in (substring in 0 (match-beginning 0)))
      (setq need-space t))
    out))

(defun clean-format (fmt result div)
  ;; If the result is a multiple of div, return just the integer. If
  ;; not, return a float. This lets you know if it is exact.
  ;; e.g. 0x7ff = 1.999k 0x800 = 2K 0x801 = 2.001K
  (if (eq (mod result div) 0)
      (format " %d%s" (/ result div) fmt)
    (format " %.3f%s" (/ result (float div)) fmt)))

;;;###autoload
(defun my-calc (command)
  "Simple calculator.

Supports the following operations:
	( )	grouping
	* / %	multiplication, division, modulo
	+  -	addition and subtraction
	<< >>	arithmetic shift left and right
	&	bitwise and
	^	bitwise exclusive or
	|	bitwise or

Numbers are as follows:
	0N      is an octal (base 8) number
	0xN	is a hex (base 16) number
	[-]N.N	is a floating point number
	[-]N	is an integer

	Commas are allowed; as are a $ at the start or end of a number.

Numbers can also have a suffix:

	k or K	for kilobyte (1024)
	m or M	for megabyte (1024 * 1024)
	g or G	for gigabyte (1024 * 1024 * 1024)
	t or T	for terabyte (1024 * 1024 * 1024 * 1024)
	p or P  for page     (4096)

If you put =suffix at the end of the command, it outputs the
result in the given unit.

Output goes to the *calc* buffer and the echo line."
  (interactive "sCalc: ")
  (save-current-buffer
    (set-buffer (get-buffer-create "*calc*"))
    (save-restriction
      (narrow-to-region (point-max) (point-max))
      (insert command)
      (insert ?=)			; mark end of command

      ;; Remove all white space, commas, $, and Ns
      (goto-char (point-min))
      (while (re-search-forward "[ \tN,$]" nil t) (replace-match ""))
      (goto-char (point-min))		; go back to start

      (let (ops cmd result one two func str format)

	(stack-clear calc-nums)
	(setq ops (stack-create))
	(stack-push ops ?=)

	;; Continue until all input parsed and command stack empty.
	(while (or (not (eq (following-char) ?=)) (not (eq (stack-top ops) ?=)))

	  ;; special case for two char tokens `>>' and `<<'
	  (if (looking-at "\\(<<\\)\\|\\(>>\\)") (forward-char))

	  ;; process a command char
	  (if (<= (my-calc-f (stack-top ops)) (my-calc-g (following-char) t))
	      ;; shift
	      (progn
		(stack-push ops (following-char))
		(if (not (eq (following-char) ?=)) (forward-char)))
	    ;; reduce
	    (while
		(progn
		  ;; Pop the top command.
		  (setq cmd (stack-pop ops))

		  (when (setq func (cdr (assoc cmd my-calc-commands)))
		    ;;Perform operation and push result
		    ;; All commands take two numbers
		    (setq two (stack-pop calc-nums))
		    (setq one (stack-pop calc-nums))
		    (stack-push calc-nums (apply func one two nil)))

		  ;; Until
		  (>= (my-calc-f (stack-top ops)) (my-calc-g cmd))))))

	(forward-char) ;; skip over =
	(setq result (stack-pop calc-nums))
	(cond
	 ((integerp result)
	  (setq format
		(cond
		 ((looking-at "[kK]") (clean-format "K" result 1024))
		 ((looking-at "[pP]") (clean-format "P" result 4096))
		 ((looking-at "[mM]") (clean-format "M" result 1048576))
		 ((looking-at "[gG]") (clean-format "G" result 1073741824))
		 ((looking-at "[tT]") (clean-format "T" result 1099511627776))
		 (t "")))
	  (setq str (format "%s = %d (%x %o)%s\n" command result result result format))
	  (message "%s (%s  %o)%s"  (my-calc-comma result) (my-calc-hex result) result format))
	 ((floatp result)
	  (setq str (format "%s = %f\n" command result))
	  (message "%s" (my-calc-float-comma result)))
	 (t	;; Huh? Should be integer or float...
	  (setq str (format "%s = ?%S?\n" command result))
	  (message "?%S?" result)))

	;; Done! Put the result in the *calc* buffer
	(delete-region (point-min) (point-max))
	(insert str)))))

;;}}}

;;; my-calc.el ends here
