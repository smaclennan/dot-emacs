;;; smerge.el --- SAM's Merge layer on top of ediff

;; Copyright (C) 2002-2019 Sean MacLennan

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

;; 			      Introduction
;;
;; I am a huge fan of ediff. Once you are used to markups of changes within
;; the lines, it is hard to go back to other diff/merge tools. But while
;; ediff is great for buffers, files, single directories, and 3 way merges,
;; it falls down on large directory structures with small changes all over
;; the place. You spend too much time descending into directories with no
;; changes.
;;
;; So I wrote smerge. Smerge handles finding all the changes between two
;; directories. It then uses ediff to do the real work.
;;
;; 			      How it Works
;;
;; You give smerge two directories to start with. It then calls `diff -r
;; --brief' on the two directories. The output is post-processed into three
;; columns: only in directory 1, in both but different, only in directory
;; 2.
;;
;; In the left and right columns, you can copy the file to the other
;; directory. In the middle column, mouse 1 and mouse 2 call ediff on the
;; two files.

(require 'ediff)

(defvar smerge-diff-program ediff-diff-program
  "*Program to use to diff the directories. Must support --brief option.")

(defvar smerge-diff-options "-w"
  "*Options used to compare directories. See `ediff-diff-options'.")

(defvar smerge-ediff-options nil
  "*Options used to compare files that differ.  Nil means use
`ediff-diff-options'. You must call `ediff-set-actual-diff-options'
after changing this.")

(defvar smerge-diff-excludes '("*.o" "*.a" "*~" ".#*" ".svn"
	".git" "*.cmd" "*.lo" "*.ko" ".tmp_versions" "*.Plo"
	"modules.order" "*.elc" "*.mod.c" "TAGS" "*.builtin")
  "*List of patterns of files and subdirectories to ignore.
smerge builds a temprorary file (`smerge-exclude-file') based on this list
and passes it to `smerge-diff-program' with the --exclude-from option.
Note: These excludes are wildcard expressions as used by diff, not lisp
regular expressions.")

(defvar smerge-only-in-excludes nil
  "*List of regular expressions to exclude from the only-in lists.")

(defvar smerge-preserve-modes t
  "*When copying files, preserve the mode of the destination file.")

(defvar smerge-exclude-file "/tmp/smerge-excludes"
  "*Temporary file to hold the `smerge-excludes'.")

(defgroup smerge nil
  "Faces for highlighting in smerge."
  :prefix "smerge-"
  :group 'tools)

(defface smerge-only1-face
  '((((class color))  (:foreground "purple"))
    (t (:underline t)))
  "Face for files/directories only in directory 1.")

(defface smerge-only2-face
  '((((class color))  (:foreground "blue"))
    (t (:underline t)))
  "Face for files/directories only in directory 2.")

(defface smerge-diff-face
  '((((class color))  (:foreground "red"))
    (t (:bold t)))
  "Face for files that are different.")

(defface smerge-merged-face
  '((((class color))  (:foreground "black")))
  "Face for files that are merged.")

(defvar smerge-buffer "*smerge-output*" "*Name of smerge output buffer.")

(define-minor-mode smerge-mode
  "Minor mode for smerge buffer."
  nil nil
  '(([mouse-1]	. smerge-ediff-or-copy)
    ([mouse-2]	. smerge-ediff-or-copy)
    ("\C-m"	. smerge-ediff-or-copy)
    ("g"	. smerge-reload)
    ("r"	. smerge-reload)
    ("n"	. smerge-next)
    ("p"	. smerge-prev)))

;; For debugging
(defvar smerge-raw-diff-output nil
  "*If non-nil, filename to write the raw diff output to. (dbg)")

;; Internals
(defvar smerge-flags nil)
(defvar smerge-dir1 nil)
(defvar smerge-dir2 nil)
(defvar smerge-file nil)
(defvar smerge-extent nil)

(defmacro overlay-at (pos) `(car (overlays-at ,pos)))

;;;###autoload
(defun smerge (flags &optional dir1 dir2)
  "Merge two directories recursively."
  (interactive "p")
  (unless dir1
    (setq dir1 (read-directory-name "Directory 1: " nil nil t)))
  (unless dir2
    (setq dir2 (read-directory-name "Directory 2: " nil nil t)))
  (switch-to-buffer smerge-buffer) ;; Yes I want to be in the output buffer
  (setq buffer-read-only nil) ;; writable
  (setq smerge-mode t)
  (setq smerge-flags flags)
  (setq smerge-dir1 (file-name-as-directory (expand-file-name dir1)))
  (setq smerge-dir2 (file-name-as-directory (expand-file-name dir2)))
  (message "Please wait....")
  (smerge-recursive-diff)
  (smerge-fixup-filenames)
  (smerge-post-process flags)
  (setq buffer-read-only t) ;; read-only
  (message "Done.")
  )

(defun smerge-reload ()
  "Rediff two directories recursively."
  (interactive)
  (smerge smerge-flags smerge-dir1 smerge-dir2))

(defun smerge-recursive-diff ()
  (let (rc)
    (erase-buffer)
    (dolist (exclude smerge-diff-excludes) (insert (concat exclude "\n")))
    (write-region (point-min) (point-max) smerge-exclude-file nil 'no-message)
    (erase-buffer)
    (let ((diff-options (concat "--exclude-from=" smerge-exclude-file
			      " -r" " --brief " smerge-diff-options)))
      ;; Since we are tightly coupled with ediff, use their program!
      ;; This erases the diff buffer automatically.
      (ediff-exec-process smerge-diff-program
			  (current-buffer)
			  'synchronize
			  diff-options
			  smerge-dir1 smerge-dir2))
    (delete-file smerge-exclude-file)
    (when smerge-raw-diff-output
      (write-region (point-min) (point-max) smerge-raw-diff-output))
    (and (numberp rc) (eq rc 0))))

(defun smerge-fixup-filenames ()
  "Diff splits the `Only in' files into directory and filename.
Top level directories end in /, subdirs do not."
  (goto-char (point-min))
  (while (re-search-forward "^\\(Only in [^:]*\\)\\(.\\): " nil t)
    (if (string= (match-string 2) "/")
	(replace-match "\\1/" nil nil)
      (replace-match "\\1\\2/" nil nil))))

(defun smerge-lists ()
  "Create strings for only-in-1, only-in-2, both."
  (list (format "^Only in %s\\(.*\\)$" smerge-dir1)
	(format "^Only in %s\\(.*\\)$" smerge-dir2)
	(format "^Files %s\\(.+\\) and %s.+ differ$"
		(regexp-quote smerge-dir1)
		(regexp-quote smerge-dir2))))

(defun smerge-post-process (flags)
  (let ((list-strings (smerge-lists)) extent file start)
    (goto-char (point-min))
    (insert (format "Diff %s and %s\n\n" smerge-dir1 smerge-dir2))
    (setq start (point))

    (cond ((eq flags nil) t)
	  ((> flags 4) ;; c-u c-u
	   ;; Remove different files
	   (while (re-search-forward "^Files .*\n" nil t)
	     (replace-match "")))
	  ((> flags 1) ;; c-u
	   ;; Remove the unique files
	   (while (re-search-forward "^Only in .*\n" nil t)
	     (replace-match ""))))

    ;; Only in 1
    (goto-char (point-min))
    (while (re-search-forward (nth 0 list-strings) nil t)
      (setq file (match-string 1))
      (save-match-data
	(cl-loop for exclude in smerge-only-in-excludes while file do
	  (when (string-match exclude file)
	    (setq file nil))))
      (if file
	  (progn
	    (setq extent
		  (smerge-make-extent (match-beginning 0) (match-end 0) 'smerge-only1-face))
	    (overlay-put extent 'type 2)
	    (replace-match file))
	(kill-whole-line))
      )

    ;; Only in 2
    (goto-char (point-min))
    (while (re-search-forward (nth 1 list-strings) nil t)
      (setq file (match-string 1))
      (save-match-data
	(cl-loop for exclude in smerge-only-in-excludes while file do
	  (when (string-match exclude file)
	    (setq file nil))))
      (if file
	  (progn
	    (setq extent
		  (smerge-make-extent (match-beginning 0) (match-end 0) 'smerge-only2-face))
	    (overlay-put extent 'type 1)
	    (replace-match (concat "\t\t\t\t" file)))
	(kill-whole-line))
      )

    ;; Both
    (goto-char (point-min))
    (while (re-search-forward (nth 2 list-strings) nil t)
      (setq file (match-string 1))
      (setq extent
	    (smerge-make-extent (match-beginning 0) (match-end 0) 'smerge-diff-face))
      (overlay-put extent 'type 3)
      (replace-match (concat "\t\t" file))
      )

    ;; Back to start
    (goto-char start)
    (if (re-search-forward "\\w" nil t) (forward-char -1))
    ))

(autoload 'defadvice "advice" nil nil 'macro)

(defun next-overlay (extent)
  (let* ((pos (1- (overlay-end extent)))
	 (next (next-overlay-change pos))
	 (overlay (overlays-at next)))
    (when overlay
      (setq overlay (car overlay))
      (when (eq overlay extent) (error "PROBLEMS"))
      )
    overlay))

(defadvice ediff-quit (after smerge activate)
  (when (overlayp smerge-extent)
    (overlay-put smerge-extent 'face 'smerge-merged-face)
    (delete-other-windows)
    (switch-to-buffer smerge-buffer)
    (let ((next (next-overlay smerge-extent))
	  start)
      (when next
	(setq start (overlay-start next))
	(goto-char start)
	(if (re-search-forward "\\w" nil t) (forward-char -1))
	))
    (setq smerge-extent nil) ;; done
    ))

(defun smerge-file (extent)
  "Given a smerge extent, return the file name."
  (let ((file (buffer-substring
	       (overlay-start extent)
	       (overlay-end extent))))
    (string-match "\t*\\(.*\\)" file)
    (match-string 1 file)))

(defun smerge-nearest-extent (pos)
  "Find the extent nearest pos. Can return nil."
  (let ((extent (overlay-at pos)))
    (unless extent
      (setq extent (overlay-at (next-overlay-change pos)))
      (unless extent
	(setq extent (overlay-at (previous-overlay-change pos)))
	))
    extent))

(defun smerge-ediff-or-copy ()
  "Ediff or copy the file."
  (interactive)
  (let* ((extent (smerge-nearest-extent (point)))
	 (type (overlay-get extent 'type)))
    (unless extent (error "No extent at point"))
    (cond ((or (eq type 1) (eq type 2))
	   (setq smerge-file (smerge-file extent))
	   (setq smerge-extent extent)
	   (smerge-copy type))
	  ((eq type 3) (smerge-ediff extent))
	  (t (beep)))))

(defun smerge-ediff (&optional extent)
  "Ediff the two files."
  (interactive)
  (let (file)
    (unless extent
      (setq extent (overlay-at (point)))
      (unless extent (error "No extent at point")))
    (unless (eq (overlay-get extent 'type) 3)
      (error "Smerge internal error. Wrong extent type."))
    (setq smerge-extent extent)
    (setq file (smerge-file extent))
    (let ((ediff-diff-options (or smerge-ediff-options ediff-diff-options)))
      (ediff-files
       (concat smerge-dir1 file) (concat smerge-dir2 file)))))

(defun smerge-copy-file (src dst)
  "Copy file preserving the destination modes."
  (let ((modes (file-modes dst)))
    (copy-file src dst t t)
    (and smerge-preserve-modes
	 modes
	 (set-file-modes dst modes))))

(defun smerge-copy (dir)
  "Do the copy to the directory specified."
  (let ((file1 (concat smerge-dir1 smerge-file))
	(file2 (concat smerge-dir2 smerge-file))
	src dst)
    (cond ((eq dir 1) (setq src file2 dst file1))
	  ((eq dir 2) (setq src file1 dst file2))
	  (t (error "Huh?")))
    (when (yes-or-no-p (format "Copy to %s? " dst))
      (smerge-copy-file src dst)
      ;; Mark as merged
      (overlay-put smerge-extent 'face 'smerge-merged-face)
      ;; If this is an "only" mark as copied
      (when (< (overlay-get smerge-extent 'type) 3)
	(overlay-put smerge-extent 'type 0))
      (setq smerge-extent nil))))

(defun smerge-make-extent (start end face)
  (let ((extent (make-overlay start end)))
    (overlay-put extent 'face face)
    (overlay-put extent 'mouse-face 'highlight)
    (overlay-put extent 'keymap smerge-mode-map)
    extent))

(provide 'smerge)
