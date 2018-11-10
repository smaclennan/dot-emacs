;;; Save all the backup files in one directory instead of willy-nilly.

;; Copyright (C) 2001 Sean MacLennan
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

;; Note this is really an interface to auto-save using a different
;; directory. These functions override the ones in files.el.
;;
;; I add the following to my crontab to delete the backup files when
;; they are more than a week old:
;; 13 5 * * * find $HOME/.backup -mtime +7 -delete

(defvar backup-directory "~/.backup/")

(defvar backup-directory-fallback "~/.backup/")

(defvar backup-hash-directory
  (expand-file-name "hash/" (or backup-directory
				backup-directory-fallback))
  "If non-nil, directory used for hashed backup filenames.")

(defvar backup-hash-p nil)

(defun make-backup-file-name (file)
  (let ((auto-save-directory backup-directory)
	(auto-save-directory-fallback backup-directory-fallback)
	(auto-save-hash-directory backup-hash-directory)
	(auto-save-hash-p backup-hash-p))
    (make-auto-save-file-name file)))

(defun backup-file-name-p (file)
  (let ((auto-save-directory backup-directory)
	(auto-save-directory-fallback backup-directory-fallback)
	(auto-save-hash-directory backup-hash-directory)
	(auto-save-hash-p backup-hash-p))
    (auto-save-file-name-p file)))

;;;###autoload
(defun backup-file-name ()
  (interactive)
  (let ((file (read-file-name "File name: " "" nil nil (buffer-file-name))))
    (message "%s" (make-backup-file-name (expand-file-name file)))))

;; Might as well put auto-save here too
(when (would-like 'auto-save)
  (setq auto-save-directory "~/.autosave/")
  ;; Now that we have auto-save-timeout, let's crank this up
  ;; for better interactive response.
  (setq auto-save-interval 2000))

(provide 'backup)
