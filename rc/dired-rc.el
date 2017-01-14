;;; dired-extras.el --- Additions to dired

;; Copyright (C) 1997-2000 Sean MacLennan
;; Revision:   1.0

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

(defvar dired-du-command "du" "*`du' command.")
(defvar dired-du-args    "-s" "*Arguments to du.")

(define-key dired-mode-map "\C-cu" 'dired-do-du)
(define-key dired-mode-map "\C-ca" 'dired-toggle-all)

(setq dired-listing-switches "-l")

(setq dired-no-confirm '(kill-file-buffer))

;;;###autoload
(defun dired-do-du ()
  "Call `du -s' on the current directory or file."
  (interactive)
  (let ((dir (dired-get-filename))
	(buf (generate-new-buffer "du")))
    (save-current-buffer
      (set-buffer buf)
      (call-process dired-du-command nil buf nil dired-du-args dir)
      (message "%s" (buffer-substring nil nil)))
    (kill-buffer buf)))

;;;###autoload
(defun dired-toggle-all (force)
  "Toggle showing dot-files."
  (interactive "P")
  (if (or force (string-match "a" dired-listing-switches))
      (while (string-match "a" dired-listing-switches)
	(setq dired-listing-switches
	      (replace-match "" nil nil dired-listing-switches)))
    (setq dired-listing-switches (concat dired-listing-switches "a")))
  (dired-sort-other dired-listing-switches))

;;;###autoload
(defun dired-do-apply-function (func)
  "Apply an interactive lisp function to all the marked files.
The function is not passed any arguments. The function honours restrictions."
  (interactive "aFunction: ")
  (save-excursion
    (dolist (file (dired-get-marked-files))
      (message "Processing %s..." file)
      (set-buffer (find-file-noselect file))
      (goto-char (point-min))
      (apply func nil))
    (message "Done.")))

;; The following is not mine but from an email to xemacs-beta
;; From: samuel padgett <res00ajf@gte.net>
;; Sender: owner-xemacs-beta@xemacs.org
;; To: xemacs-beta@xemacs.org
;; Subject: reusing dired buffer
;; Date: Thu, 29 Jun 2000 01:34:58 -0400
;;
;; <snip>
;; I would really like to have the new directory contents replace the old
;; in a dired buffer.  If my cursor is positioned over a file, however, I
;; want dired to do the normal thing: open the file in a new buffer.
;; This seems to me like the most intuitive behavior.  I hacked out some
;; lisp code that does this:

;;###autoload
(defun dired-follow-file ()
  "In dired, visit the file or directory on this line.
If a directory is on the current line, replace the current
dired buffer with one containing the contents of the directory.
Otherwise, invoke `dired-find-file' on the file."
  (interactive)
  (let ((filename (dired-get-filename)))
    ;; if the file is a directory, replace the buffer with the
    ;;  directory's contents
    (if (file-directory-p filename)
        (find-alternate-file filename)
      ;; otherwise simply perform a normal `dired-find-file'
      (dired-find-file))))

(add-hook
 'dired-mode-hook
 (lambda ()
   (local-set-key "\C-m" 'dired-follow-file)
   (local-set-key "e" 'dired-follow-file)
   (local-set-key "f" 'dired-follow-file)))

;; End of email
