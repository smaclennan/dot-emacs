;;; my-tags.el --- TAGS file handlers
;; Copyright (C) 2002-2018 Sean MacLennan
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

(require 'etags)
(provide 'my-tags)

(defvar my-tags-prog "etags"
  "* The tag program executable to use.")
(defvar my-tags-c "*.c"
  "* Extension for C files, or any other files you want in the list.")
(defvar my-tags-h "*.h"
  "* Extension for header files, or any other files you want in the
list after `my-tags-c'.")
(defvar my-tags-sh "/bin/sh"
  "* Shell to use to run tags.")

(defvar my-tags-dir nil
  "* If set, this is the directory `my-tags-tree' or `my-tags-dirs' should use.
This variable is buffer local.")
(make-variable-buffer-local 'my-tags-dir)

(defvar my-tags-buffers nil
  "* List of dirs and buffer names.")

;;;###autoload
(defun my-tags-tree (dir &optional buf tagfile)
  "Call tags on an entire tree rather than just one directory.
If BUF is defined and is not empty, it is assumed to contain lines of
files to perform tags on.
If TAGFILE is defined, the tags are put in that file."
  (interactive "DDir: ")
  (if buf
      (when (stringp buf) (setq buf (get-buffer-create buf)))
    (setq buf (get-buffer-create "*tag tree*"))
    ;; Emacs does not have (erase-buffer buf) :(
    (save-current-buffer (set-buffer buf) (erase-buffer)))

  (setq dir (file-name-as-directory (expand-file-name dir)))

  (when (eq (buffer-size buf) 0)
    ;; Doing the find in two steps seems to work better
    (call-process "find" nil (list buf nil) nil dir "-name" my-tags-c)
    (call-process "find" nil (list buf nil) nil dir "-name" my-tags-h))

  (unless tagfile (setq tagfile (concat dir "TAGS")))
  (with-current-buffer buf
    (call-process-region (point-min) (point-max)
			 my-tags-prog nil nil nil
			 "-o" tagfile "-"))
    )

;;;###autoload
(defun my-tags-simple ()
  (call-process my-tags-sh nil nil nil "-c"
		(format "%s %s %s" my-tags-prog my-tags-c my-tags-h)))

;;;###autoload
(defun my-tags-dirs ()
  "Meant to be run from `after-save-hook'."
  (when my-tags-dir
    (let ((entry (assoc my-tags-dir my-tags-buffers)))
      (my-tags-tree my-tags-dir (when entry (nth 1 entry))))))

;;;###autoload
(defun my-tags-dirs-helper (dir &optional buf)
  "Helper function to setup a directory for use with `my-tags-dirs'."
  (interactive "DDir: ")
  (setq my-tags-dir (file-name-as-directory dir))

  (make-local-variable 'tags-file-name)
  (setq tags-file-name (concat dir "TAGS"))

  (when buf
    (add-to-list 'my-tags-buffers (list dir buf)))

  ;; Make sure the tags file exists
  (unless (file-exists-p tags-file-name)
    (message "Creating tags file...")
    (my-tags-dirs))

  (add-hook 'after-save-hook 'my-tags-dirs))

;;;;;;;; --------

;; This is meant for large directories where running through the
;; entire directory tree after every save is too expensive. Instead, it
;; uses the etags append option. It deletes the old tags for the file
;; and only updates the tags for the one file that is being saved.
;;
;; Use `my-tags-update-helper' to setup for `my-tags-update'.

(defvar my-tags-file nil
  "* If set, this is the tags file `my-tags-update' should use.
This variable is buffer local.")
(make-variable-buffer-local 'my-tags-file)

(defun my-tags-remove-old-tags (tagfile fname)
  "Remove the tags from TAGFILE for FNAME."
  (let ((buf (find-file-noselect tagfile t))
	(regexp (concat "^" fname ",\\([0-9]+\\)$"))
	nchars)
    (with-current-buffer buf
      ;; Because we set no-warn on find-file-noselect, we must check
      ;; for modification ourselves
      (unless (verify-visited-file-modtime buf)
	(revert-buffer nil t))
      (goto-char (point-min))
      (when (re-search-forward regexp nil t)
	(setq nchars (string-to-number (match-string 1)))
	(kill-whole-line) ;; delete the filename line
	(delete-char nchars t)
	(if (eq (following-char) ?\f)
	    (delete-char 2 t) ;; delete \f and \n
	  (unless (eobp)
	    (undo)
	    (error "Not looking at Ctrl-L")))
	(basic-save-buffer)))))

(defun my-tags-update ()
  "Update the tags for the current file. Meant to be run from `after-save-hook'."
  (when my-tags-file
    (let ((fname (buffer-file-name)))

      ;; Remove the current tags for fname
      (my-tags-remove-old-tags my-tags-file fname)

      ;; And append the new tags
      (let ((cmd (format "%s -a -o %s %s" my-tags-prog my-tags-file fname)))
	(call-process my-tags-sh nil nil nil "-c" cmd))
      )))

;;;###autoload
(defun my-tags-refresh ()
  "Create or recreate the tags file from scratch."
  (interactive)
  (unless my-tags-file (error "`my-tags-file' not set."))
  (let (tagdir)
    (if (string-match "\\(.*/\\)TAGS$" my-tags-file)
	(setq tagdir (match-string 1 my-tags-file))
      (error "Malformed `my-tags-file'"))
    (my-tags-tree tagdir)))

;;;###autoload
(defun my-tags-update-helper (dir)
  "Helper function to setup for `my-tags-update'."
  (interactive "DDir: ")
  (setq my-tags-file (concat (file-name-as-directory dir) "TAGS"))

  (set (make-local-variable 'tags-file-name) my-tags-file)

  ;; Make sure the tags file exists
  (unless (file-exists-p tags-file-name)
    (message "Creating tags file...")
    (my-tags-tree dir))

  (add-hook 'after-save-hook 'my-tags-update))
