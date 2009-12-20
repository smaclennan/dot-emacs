;;; itimer.el - itimer compatibility for Emacs

;; Copyright (C) 2002-2009 Sean MacLennan
;; Revision:   1.1
;; Emacs

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABIL`ITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; This is not a complete implementation of itimers. It is
;; only enough for slashdot.el.

(defvar itimer-list nil)

(defun start-itimer (name function value &optional restart)
  "Start an itimer."
  (when (itimer-live-p name) (error "%s already a timer." name))
  (let ((timer (timer-create)))
    (setq itimer-list (cons (list name timer) itimer-list))
    (timer-set-function timer function)
    (timer-set-time timer
		    (timer-relative-time (current-time) value)
		    restart)
    (timer-activate timer)
    timer))

(defun get-itimer (name)
  "Return itimer named NAME, or nil if there is none."
  (let ((itimer (assoc name itimer-list)))
    (if itimer
	(nth 1 itimer)
      nil)))

;; SAM Does not return if active, only if exists.
;; Always returns timer or nil.
;; This is good enough for slashdot.el
(defun itimer-live-p (object)
  "Return non-nil if OBJECT is an itimer and is active."
  (if (stringp object)
      (let ((itimer (assoc object itimer-list)))
	(if itimer
	    (nth 1 itimer)
	  nil))
    (if (timerp object)
	object
      nil)))

(defun delete-itimer (itimer)
  (let ((timer (itimer-live-p itimer)))
    (when timer
      (cancel-timer timer)
      (dolist (elt itimer-list)
	(if (eq (nth 1 elt) timer)
	    (setq itimer-list (delete elt itimer-list)))))))

(provide 'itimer)
