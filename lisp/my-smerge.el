;;; my-smerge.el --- SAM's Merge layer on top of ediff

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
;; So I wrote my-smerge. My-Smerge handles finding all the changes between two
;; directories. It then uses ediff to do the real work.
;;
;; 			      How it Works
;;
;; You give my-smerge two directories to start with. It then calls `diff -r
;; --brief' on the two directories. The output is post-processed into three
;; columns: only in directory 1, in both but different, only in directory
;; 2.
;;
;; In the left and right columns, you can copy the file to the other
;; directory. In the middle column, mouse 1 and mouse 2 call ediff on the
;; two files.

(require 'ediff)

(defvar my-smerge-diff-program ediff-diff-program
  "*Program to use to diff the directories. Must support --brief option.")

(defvar my-smerge-diff-options "-w"
  "*Options used to compare directories. See `ediff-diff-options'.")

(defvar my-smerge-ediff-options nil
  "*Options used to compare files that differ.  Nil means use
`ediff-diff-options'. You must call `ediff-set-actual-diff-options'
after changing this.")

(defvar my-smerge-diff-excludes '("*.o" "*.a" "*~" ".#*" ".svn"
	".git" "*.cmd" "*.lo" "*.ko" ".tmp_versions" "*.Plo"
	"modules.order" "*.elc" "*.mod.c" "TAGS" "*.builtin")
  "*List of patterns of files and subdirectories to ignore.
my-smerge builds a temprorary file (`my-smerge-exclude-file') based on this list
and passes it to `my-smerge-diff-program' with the --exclude-from option.
Note: These excludes are wildcard expressions as used by diff, not lisp
regular expressions.")

(defvar my-smerge-only-in-excludes nil
  "*List of regular expressions to exclude from the only-in lists.")

(defvar my-smerge-preserve-modes t
  "*When copying files, preserve the mode of the destination file.")

(defvar my-smerge-exclude-file "/tmp/my-smerge-excludes"
  "*Temporary file to hold the `my-smerge-excludes'.")

(defgroup my-smerge nil
  "Faces for highlighting in my-smerge."
  :prefix "my-smerge-"
  :group 'tools)

(defface my-smerge-only1-face
  '((((class color))  (:foreground "purple"))
    (t (:underline t)))
  "Face for files/directories only in directory 1.")

(defface my-smerge-only2-face
  '((((class color))  (:foreground "blue"))
    (t (:underline t)))
  "Face for files/directories only in directory 2.")

(defface my-smerge-diff-face
  '((((class color))  (:foreground "red"))
    (t (:bold t)))
  "Face for files that are different.")

(defface my-smerge-merged-face
  '((((class color))  (:foreground "black")))
  "Face for files that are merged.")

(defvar my-smerge-buffer "*smerge-output*" "*Name of my-smerge output buffer.")

(define-minor-mode my-smerge-mode
  "Minor mode for my-smerge buffer."
  :lighter " my-smerge"
  :keymap '(("\C-m"	. my-smerge-ediff-or-copy)
	    ("g"	. my-smerge-reload)
	    ("r"	. my-smerge-reload)
	    ("n"	. my-smerge-next)
	    ))

;; For debugging
(defvar my-smerge-raw-diff-output nil
  "*If non-nil, filename to write the raw diff output to. (dbg)")

;; Internals
(defvar my-smerge-flags nil)
(defvar my-smerge-dir1 nil)
(defvar my-smerge-dir2 nil)
(defvar my-smerge-file nil)
(defvar my-smerge-extent nil)

(defmacro overlay-at (pos) `(car (overlays-at ,pos)))

;;;###autoload
(defun my-smerge (flags &optional dir1 dir2)
  "Merge two directories recursively."
  (interactive "p")
  (unless dir1
    (setq dir1 (read-directory-name "Directory 1: " nil nil t)))
  (unless dir2
    (setq dir2 (read-directory-name "Directory 2: " nil nil t)))
  (run-hooks 'my-smerge-mode-hook)
  (switch-to-buffer my-smerge-buffer) ;; Yes I want to be in the output buffer
  (setq buffer-read-only nil) ;; writable
  (setq my-smerge-mode t)
  (setq my-smerge-flags flags)
  (setq my-smerge-dir1 (file-name-as-directory (expand-file-name dir1)))
  (setq my-smerge-dir2 (file-name-as-directory (expand-file-name dir2)))
  (message "Please wait....")
  (my-smerge-recursive-diff)
  (my-smerge-fixup-filenames)
  (my-smerge-post-process flags)
  (setq buffer-read-only t) ;; read-only
  (message "Done.")
  )

(defun my-smerge-reload ()
  "Rediff two directories recursively."
  (interactive)
  (my-smerge my-smerge-flags my-smerge-dir1 my-smerge-dir2))

(defun my-smerge-recursive-diff ()
  (let (rc)
    (erase-buffer)
    (dolist (exclude my-smerge-diff-excludes) (insert (concat exclude "\n")))
    (write-region (point-min) (point-max) my-smerge-exclude-file nil 'no-message)
    (erase-buffer)
    (let ((diff-options (concat "--exclude-from=" my-smerge-exclude-file
			      " -r" " --brief " my-smerge-diff-options)))
      ;; Since we are tightly coupled with ediff, use their program!
      ;; This erases the diff buffer automatically.
      (ediff-exec-process my-smerge-diff-program
			  (current-buffer)
			  'synchronize
			  diff-options
			  my-smerge-dir1 my-smerge-dir2))
    (delete-file my-smerge-exclude-file)
    (when my-smerge-raw-diff-output
      (write-region (point-min) (point-max) my-smerge-raw-diff-output))
    (and (numberp rc) (eq rc 0))))

(defun my-smerge-fixup-filenames ()
  "Diff splits the `Only in' files into directory and filename.
Top level directories end in /, subdirs do not."
  (goto-char (point-min))
  (while (re-search-forward "^\\(Only in [^:]*\\)\\(.\\): " nil t)
    (if (string= (match-string 2) "/")
	(replace-match "\\1/" nil nil)
      (replace-match "\\1\\2/" nil nil))))

(defun my-smerge-lists ()
  "Create strings for only-in-1, only-in-2, both."
  (list (format "^Only in %s\\(.*\\)$" my-smerge-dir1)
	(format "^Only in %s\\(.*\\)$" my-smerge-dir2)
	(format "^Files %s\\(.+\\) and %s.+ differ$"
		(regexp-quote my-smerge-dir1)
		(regexp-quote my-smerge-dir2))))

(defun my-smerge-post-process (flags)
  (let ((list-strings (my-smerge-lists)) extent file start)
    (goto-char (point-min))
    (insert (format "Diff %s and %s\n\n" my-smerge-dir1 my-smerge-dir2))
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
	(cl-loop for exclude in my-smerge-only-in-excludes while file do
	  (when (string-match exclude file)
	    (setq file nil))))
      (if file
	  (progn
	    (setq extent
		  (my-smerge-make-extent (match-beginning 0) (match-end 0) 'my-smerge-only1-face))
	    (overlay-put extent 'type 2)
	    (replace-match file))
	(kill-whole-line))
      )

    ;; Only in 2
    (goto-char (point-min))
    (while (re-search-forward (nth 1 list-strings) nil t)
      (setq file (match-string 1))
      (save-match-data
	(cl-loop for exclude in my-smerge-only-in-excludes while file do
	  (when (string-match exclude file)
	    (setq file nil))))
      (if file
	  (progn
	    (setq extent
		  (my-smerge-make-extent (match-beginning 0) (match-end 0) 'my-smerge-only2-face))
	    (overlay-put extent 'type 1)
	    (replace-match (concat "\t\t\t\t" file)))
	(kill-whole-line))
      )

    ;; Both
    (goto-char (point-min))
    (while (re-search-forward (nth 2 list-strings) nil t)
      (setq file (match-string 1))
      (setq extent
	    (my-smerge-make-extent (match-beginning 0) (match-end 0) 'my-smerge-diff-face))
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

(defadvice ediff-quit (after my-smerge activate)
  (when (overlayp my-smerge-extent)
    (overlay-put my-smerge-extent 'face 'my-smerge-merged-face)
    (delete-other-windows)
    (switch-to-buffer my-smerge-buffer)
    (let ((next (next-overlay my-smerge-extent))
	  start)
      (when next
	(setq start (overlay-start next))
	(goto-char start)
	(if (re-search-forward "\\w" nil t) (forward-char -1))
	))
    (setq my-smerge-extent nil) ;; done
    ))

(defun my-smerge-file (extent)
  "Given a my-smerge extent, return the file name."
  (let ((file (buffer-substring
	       (overlay-start extent)
	       (overlay-end extent))))
    (string-match "\t*\\(.*\\)" file)
    (match-string 1 file)))

(defun my-smerge-nearest-extent (pos)
  "Find the extent nearest pos. Can return nil."
  (let ((extent (overlay-at pos)))
    (unless extent
      (setq extent (overlay-at (next-overlay-change pos)))
      (unless extent
	(setq extent (overlay-at (previous-overlay-change pos)))
	))
    extent))

(defun my-smerge-ediff-or-copy ()
  "Ediff or copy the file."
  (interactive)
  (let* ((extent (my-smerge-nearest-extent (point)))
	 (type (overlay-get extent 'type)))
    (unless extent (error "No extent at point"))
    (cond ((or (eq type 1) (eq type 2))
	   (setq my-smerge-file (my-smerge-file extent))
	   (setq my-smerge-extent extent)
	   (my-smerge-copy type))
	  ((eq type 3) (my-smerge-ediff extent))
	  (t (beep)))))

(defun my-smerge-next ()
  "Move to next diff line."
  (interactive)
  (forward-line 1)
  (my-smerge-ediff-or-copy))

(defun my-smerge-ediff (&optional extent)
  "Ediff the two files."
  (interactive)
  (let (file)
    (unless extent
      (setq extent (overlay-at (point)))
      (unless extent (error "No extent at point")))
    (unless (eq (overlay-get extent 'type) 3)
      (error "My-Smerge internal error. Wrong extent type."))
    (setq my-smerge-extent extent)
    (setq file (my-smerge-file extent))
    (let ((ediff-diff-options (or my-smerge-ediff-options ediff-diff-options)))
      (ediff-files
       (concat my-smerge-dir1 file) (concat my-smerge-dir2 file)))))

(defun my-smerge-copy-file (src dst)
  "Copy file preserving the destination modes."
  (let ((modes (file-modes dst)))
    (if (file-directory-p src)
	(copy-directory src dst t t)
      (copy-file src dst t t))
    (and my-smerge-preserve-modes
	 modes
	 (set-file-modes dst modes))))

(defun my-smerge-copy (dir)
  "Do the copy to the directory specified."
  (let ((file1 (concat my-smerge-dir1 my-smerge-file))
	(file2 (concat my-smerge-dir2 my-smerge-file))
	src dst)
    (cond ((eq dir 1) (setq src file2 dst file1))
	  ((eq dir 2) (setq src file1 dst file2))
	  (t (error "Huh?")))
    (when (yes-or-no-p (format "Copy to %s? " dst))
      (my-smerge-copy-file src dst)
      ;; Mark as merged
      (overlay-put my-smerge-extent 'face 'my-smerge-merged-face)
      ;; If this is an "only" mark as copied
      (when (< (overlay-get my-smerge-extent 'type) 3)
	(overlay-put my-smerge-extent 'type 0))
      (setq my-smerge-extent nil))))

(defun my-smerge-make-extent (start end face &optional keymap)
  (unless keymap (setq keymap my-smerge-mode-map))
  (let ((extent (make-overlay start end)))
    (overlay-put extent 'face face)
    (overlay-put extent 'mouse-face 'highlight)
    (overlay-put extent 'keymap keymap)
    extent))

(provide 'my-smerge)
