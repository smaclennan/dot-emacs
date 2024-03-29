;; -*- lexical-binding: t -*-
;;; trim.el --- Various multi-line operations

;; Copyright (C) 1998,2002 Sean MacLennan
;; $Revision: 1.3 $ $Date: 2003/03/19 02:53:29 $

;; License agrement

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

;; This function does the actual triming.
;; All the others are calls to this with predefined regexps
;;;###autoload
(defun trim (regexp &optional replace)
  "Searches for REGEXP and deletes any matches.
If REPLACE is non-nil, it replaces the match.
If the region is active, only trims the lines in the region."
  (interactive "*sRegexp: ")
  (save-excursion
    (save-restriction
      (if mark-active
	  (narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (if (not replace) (setq replace ""))
      (while (re-search-forward regexp nil t)
	(replace-match replace)))))

;;;###autoload
(defun trim-lines ()
  "Trim whitespace (including CR) from the ends of all lines.
If the region is active, only trims the lines in the region."
  (interactive "*")
  (trim "[ \t\r]+$"))

;;;###autoload
(defun trim-empty-lines ()
  "Trim all empty lines.
An empty line is a line with only whitespace (space, tab, CR) characters.
If the region is active, only trims the lines in the region."
  (interactive "*")
  (trim "^[ \t]*[\r]?\n"))

;; Regex from `single-lines-only' by Trey Jackson
;; https://stackoverflow.com/questions/4419576/delete-extra-blank-lines-in-emacs
(defun trim-multiple-lines (&optional full-cleanup)
  "Trim multiple empty lines to one empty line.
If `full-cleanup' is non-nil, also remove possible empty line at start
and end of the buffer. If the region is active, only trims the lines
in the region."
  (interactive "P")
  (save-excursion
    (save-restriction
      (when mark-active
	(narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (while (re-search-forward "\\(^\\s-*$\\)\n" nil t)
	(replace-match "\n")
	(unless (eobp) (forward-char 1)))
      (when full-cleanup
	(goto-char (point-min))
	;; Remove possible empty line at start of file
	(when (eolp) (delete-char 1))
	;; Remove possible empty line at end of file
	(goto-char (point-max))
	(when (bolp) (delete-char -1))
	))))

;;;###autoload
(defun trim-spaces ()
  "Trim all sequences of spaces to one space.
If the region is active, only trims the lines in the region."
  (interactive "*")
  (trim " +" " "))

;;;###autoload
(defun trim-leading-spaces ()
  "Trim all white space at the starts of lines.
If the region is active, only trims the lines in the region."
  (interactive "*")
  (trim "^[ \t]+"))

;;;###autoload
(defun trim-defines ()
  "Trim `#define' lines to isolate the `identifier'.
If the region is active, only trims the lines in the region."
  (interactive "*")
  (trim "^[ \t]*#define[ \t]+\\([^ \t(]*\\).*" "\\1"))

;;;###autoload
(defun trim-case ()
  "Trim `case X:' lines to isolate the `identifier'.
If the region is active, only trims the lines in the region."
  (interactive "*")
  (trim "^[ \t]*case[ \t]+\\([a-zA-Z0-9_]+\\).*" "\\1"))

;;;###autoload
(defun trim-sh-comments ()
  "Trim all full length shell comments.
If the region is active, only trims the lines in the region."
  (interactive "*")
  (trim "^#.*$"))

;;;###autoload
(defun dos2unix ()
  "Convert a DOS file to Unix format."
  (interactive "*")
  (set-buffer-file-coding-system 'raw-text-unix t)
  (trim "\r"))

;;;###autoload
(defun unix2dos ()
  "Add ^M to the end of all lines.
If the region is active, only untrims the lines in the region."
  (interactive)
  (save-excursion
    (save-restriction
      (if mark-active
	  (narrow-to-region (region-beginning) (region-end)))
      (goto-char (point-min))
      (while (not (eobp))
	(end-of-line)
	(unless (eq (char-before) ?\r)
	  (insert ?\r))
	(forward-char)))))

(provide 'trim)
