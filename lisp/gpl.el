;; -*- lexical-binding: t -*-
;; gpl.el - Add a GPL notice to a file
;; Copyright (C) 2010 Sean MacLennan <seanm@seanm.ca>
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

(defvar gpl-2
  '("This program is free software; you can redistribute it and/or modify"
    "it under the terms of the GNU General Public License as published by"
    "the Free Software Foundation, either version 2 of the License, or"
    "(at your option) any later version."
    ""
    "This program is distributed in the hope that it will be useful,"
    "but WITHOUT ANY WARRANTY; without even the implied warranty of"
    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
    "GNU General Public License for more details."
    ""
    "You should have received a copy of the GNU General Public License"
    "along with this project; see the file COPYING.  If not, write to"
    "the Free Software Foundation, Inc., 59 Temple Place - Suite 330,"
    "Boston, MA 02111-1307, USA.")
  "GPL2 text as a list of lines.")

(defvar gpl-3
  '("This program is free software; you can redistribute it and/or modify"
    "it under the terms of the GNU General Public License as published by"
    "the Free Software Foundation, either version 3 of the License, or"
    "(at your option) any later version."
    ""
    "This program is distributed in the hope that it will be useful,"
    "but WITHOUT ANY WARRANTY; without even the implied warranty of"
    "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
    "GNU General Public License for more details."
    ""
    "You should have received a copy of the GNU General Public License"
    "along with this program.  If not, see <http://www.gnu.org/licenses/>.")
  "GPL3 text as a list of lines.")

(defvar gpl-year-str nil
  "* This string will be used for the copyright year unless
specified as an arg. If nil, use the current year.")

(defvar gpl-name nil
  "* This string will be used for the copyright name unless specified
as an arg. If nil, use `user-full-name'.")

(defvar gpl-email nil
  "* This string will be used for the email unless specified as an
arg. If nil, use `user-mail-address'.")

(defvar comment-mode-list
  '((emacs-lisp-mode ";;")
    (c-mode "/*" " *" " */")
    (c++-mode "/*" " *" " */")
    (perl-mode "#")
    (python-mode "#")
    )
  "* List of modes and comment characters.")

(defun gpl-internal (gpl &optional year-str name email)
  "This does the work of adding a copyleft notice."
  (unless year-str
    (if gpl-year-str
	(setq year-str gpl-year-str)
      (setq year-str (substring (current-time-string) 20))))
  (unless name
    (if gpl-name
	(setq name gpl-name)
      (setq name user-full-name)))
  (unless email
    (if gpl-email
	(setq email gpl-email)
      (setq email user-mail-address)))

  (goto-char (point-min))
  (when (search-forward "GNU General Public License" 1000 t)
    (error "You already have a GPL license"))

  (let ((fname (file-name-nondirectory (buffer-file-name)))
	save comment-start comment-middle comment-end)
    (unless fname
      (error "Buffer is not associated with a file!"))
    (dolist (mode comment-mode-list)
      (when (eq (car mode) major-mode)
	(setq comment-start  (nth 1 mode)
	      comment-middle (nth 2 mode)
	      comment-end    (nth 3 mode))))
    (unless comment-start
      (error "Unable to match mode %S" major-mode))
    (unless comment-middle
      (setq comment-middle comment-start))

    ;; Delete any blank lines at the top of the file
    (while (looking-at "^[ \t]*$") (kill-line))

    (insert (format "%s %s - " comment-start fname))
    (setq save (point))
    (insert (format "\n%s Copyright (C) %s %s%s\n"
		    comment-middle year-str name
		    (if email (concat " <" email ">") "")))
    (insert comment-middle "\n")
    (dolist (line gpl)
      (insert comment-middle " " line "\n"))
    (if comment-end
	(insert comment-end "\n\n")
      (insert "\n"))
    (goto-char save)))

;;;###autoload
(defun gpl2 (&optional year-str name email)
  "Add a GPL2 copyleft notice to the top of the file."
  (interactive)
  (gpl-internal gpl-2 year-str name email))

;;;###autoload
(defun gpl3 (&optional year-str name email)
  "Add a GPL3 copyleft notice to the top of the file."
  (interactive)
  (gpl-internal gpl-3 year-str name email))

;;;###autoload
(defun gpl-update (&optional year-str)
  "Try to update the year in the Copyright string.
Note: Moves the point on purpose so you can see the change (or lack thereof)."
  (interactive)
  (unless year-str
    (if gpl-year-str
	(setq year-str gpl-year-str)
      (setq year-str (substring (current-time-string) 20))))
  (goto-char (point-min))
  (forward-line 10)
  (unless (re-search-backward "Copyright ([cC]) " nil t)
    (error "%s has no copyright" (buffer-name)))
  (goto-char (match-end 0))
  (when (looking-at "[0-9]+-")
    (goto-char (match-end 0)))
  (if (looking-at "[0-9]+")
      (replace-match year-str)
    (error "No year match")))

;;;###autoload
(defun gpl-git-update ()
  (interactive)
  (let ((file (buffer-file-name))
	last year)
    (setq last (split-string
		(shell-command-to-string
		 (concat "git log -1 --format=%cd " file))))
    (when (string= (nth 0 last) "fatal:")
      (error "%s not in git" file))
    (setq year (nth 4 last))
    (gpl-update year)))

;;;###autoload
(defun gpl-git-update-file (file)
  (interactive "fFile: ")
  (let ((buf (find-file-noselect file)))
    (with-current-buffer buf
      (gpl-git-update))))

(provide 'gpl)
