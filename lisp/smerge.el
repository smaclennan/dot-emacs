;;; smerge.el --- SAM's Merge layer on top of ediff

;; Copyright (C) 2002-2009 Sean MacLennan
;; XEmacs

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

;; TODO:
;;      - windows way could work in general case
;;      - keymap for buffer!
;;	- ediff needs a list of args

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
;; two files. mouse 3 opens a menu that lets you copy the file to the other
;; directory.
;;
;; Warning for windows users: You must have:
;;     (setq directory-sep-char ?/)
;; somewhere before running smerge.

(eval-when-compile (require 'cl))
(require 'ediff)

(defmacro smerge-feature-cond (&rest clauses)
  "Test CLAUSES for feature at compile time.
Each clause is (FEATURE BODY...)."
  (dolist (x clauses)
    (let ((feature (car x))
	  (body (cdr x)))
      (when (or (eq feature t)
		(featurep feature))
	(return (cons 'progn body))))))

(smerge-feature-cond
  (xemacs
   (require 'overlay)

   (defalias 'smerge-dirlist 'directory-files)
   (defalias 'smerge-read-only 'toggle-read-only)
   (defalias 'kill-whole-line 'kill-entire-line)
   )

  (emacs
   (defalias 'smerge-read-only 'read-only-mode)

   (defun smerge-dirlist (directory &optional full match nosort files-only)
     (if (eq files-only nil)
	 (directory-files directory full match nosort)
       (let ((rawlist (directory-files-and-attributes
		       directory full match nosort))
	     dirlist)
	 (setq files-only (if (eq files-only t) nil t))
	 (dolist (entry rawlist)
	   (when (eq (nth 1 entry) files-only)
	     (setq dirlist (cons (car entry) dirlist))))
	 dirlist)))

   (defun temp-directory ()
     (let ((tmp (getenv "TMPDIR")))
       (if tmp tmp "/tmp")))

   (unless (fboundp 'read-directory-name)
     (defun read-directory-name (prompt &optional dir default mustmatch)
       (let* ((dir (read-file-name prompt dir default mustmatch))
	      (attr (file-attributes dir)))
	 (unless (eq (car attr) t) (error "Must be a directory"))
	 dir))
     )
   )) ;; emacs

(defvar smerge-diff-program ediff-diff-program
  "*Program to use to diff the directories. Must support --brief option.")

(defvar smerge-diff-options ediff-diff-options
  "*Options used to compare directories. See `ediff-diff-options'.")

(defvar smerge-ediff-options nil
  "*Options used to compare files that differ.  Nil means use
`ediff-diff-options'. You must call `ediff-set-actual-diff-options'
after changing this.")

(defvar smerge-diff-excludes '("*.o" "*.obj" "*.a" "*.lib" "*~" ".#*")
  "*List of patterns of files and subdirectories to ignore.
smerge builds a temprorary file (`smerge-exclude-file') based on this list
and passes it to `smerge-diff-program' with the --exclude-from option.
Note: These excludes are wildcard expressions as used by diff, not lisp
regular expressions.")

(defvar smerge-only-in-excludes nil
  "*List of regular expressions to exclude from the only-in lists.")

(defvar smerge-preserve-modes t
  "*When copying files, preserver the mode of the destination file.")

(defvar smerge-exclude-file (concat (temp-directory) "/smerge-excludes")
  "*Temporary file to hold the `smerge-excludes'.")

(defgroup smerge-highlighting nil
  "Faces for highlighting in smerge."
  :prefix "smerge-"
  :group 'smerge)

(defface smerge-only1-face
  '((((class color))  (:foreground "purple"))
    (t (:underline t)))
  "Face for files/directories only in directory 1."
  :group 'smerge-highlighting)

(defface smerge-only2-face
  '((((class color))  (:foreground "blue"))
    (t (:underline t)))
  "Face for files/directories only in directory 2."
  :group 'smerge-highlighting)

(defface smerge-diff-face
  '((((class color))  (:foreground "red"))
    (t (:bold t)))
  "Face for files that are different."
  :group 'smerge-highlighting)

(defface smerge-merged-face
  '((((class color))  (:foreground "black"))
    (t (:bold t)))
  "Face for files that are merged."
  :group 'smerge-highlighting)


(defvar smerge-buffer "*smerge-output*" "*Name of smerge output buffer.")

(defvar smerge-keymap nil "*Keymap used by smerge.")

;; Minor mode
(defvar smerge-mode-hook nil)
(defvar smerge-mode-on-hook nil)
(defvar smerge-mode-off-hook nil)

(define-minor-mode smerge-mode
  "Minor mode for smerge buffer."
  nil nil smerge-keymap)
;; Minor mode

;; For debugging
(defvar smerge-raw-diff-output nil
  "*If non-nil, filename to write the raw diff output to. (dbg)")

;; Internals
;; SAM This should be a list?
(defvar smerge-flags nil)
(defvar smerge-dir1 nil)
(defvar smerge-dir2 nil)
(defvar smerge-file nil)
(defvar smerge-extent nil)


(defun overlay-at (pos) (car (overlays-at pos)))

(defconst smerge-copy-menu
  (list "Copy to ..."
	[(concat smerge-dir1 smerge-file) (smerge-copy 1) (smerge-allow-dir 1)]
	[(concat smerge-dir2 smerge-file) (smerge-copy 2) (smerge-allow-dir 2)]
	))

(defun smerge-init ()
  "This creates the keymap."
  (unless smerge-keymap
    (setq smerge-keymap (make-sparse-keymap "smerge"))
    (if (featurep 'xemacs)
	(progn
	  (define-key smerge-keymap 'button1 'smerge-mousable)
	  (define-key smerge-keymap 'button2 'smerge-mousable)
	  (define-key smerge-keymap 'button3 'smerge-menu))
      (define-key smerge-keymap [mouse-1] 'smerge-mousable)
      (define-key smerge-keymap [mouse-2] 'smerge-mousable)
      (define-key smerge-keymap [mouse-3] 'smerge-menu))

    (define-key smerge-keymap "\C-m" 'smerge-ediff-or-copy)
    (define-key smerge-keymap "g"    'smerge-reload)
    (define-key smerge-keymap "r"    'smerge-reload)
    (define-key smerge-keymap "n"    'smerge-next)
    (define-key smerge-keymap "p"    'smerge-prev)
    ))

;;;###autoload
(defun smerge (flags &optional dir1 dir2)
  "Merge two directories recursively."
  (interactive "p")
  (smerge-init)
  (unless dir1
    (setq dir1 (read-directory-name "Directory 1: " nil nil t)))
  (unless dir2
    (setq dir2 (read-directory-name "Directory 2: " nil nil t)))
  (switch-to-buffer smerge-buffer) ;; Yes I want to be in the output buffer
  (smerge-read-only 0) ;; writable
  (setq smerge-mode t)
  (setq smerge-flags flags)
  (setq smerge-dir1 (file-name-as-directory (expand-file-name dir1)))
  (setq smerge-dir2 (file-name-as-directory (expand-file-name dir2)))
  (message "Please wait....")
  (smerge-recursive-diff)
  (smerge-fixup-filenames)
  (smerge-post-process flags)
  (smerge-read-only 1) ;; read-only
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
  (if (eq system-type 'windows-nt)
      (progn
	(goto-char (point-min))
	(while (re-search-forward "\\(.\\): " nil t)
	  (if (eq (string-to-char (match-string 1)) ?/)
	      (replace-match "/") (replace-match "\\1/"))))
    (goto-char (point-min))
    (while (re-search-forward "^\\(Only in [^:]*\\)\\(.\\): " nil t)
      (if (string= (match-string 2) "/")
	  (replace-match "\\1/" nil nil)
	(replace-match "\\1\\2/" nil nil)))))

(defun smerge-post-process (flags)
  (let (only-1 only-2 both extent file start)
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


    (if (eq system-type 'windows-nt)
	(setq only-1 (format "^Only in %s\\(.*\\)$" smerge-dir1)
	      only-2 (format "^Only in %s\\(.*\\)$" smerge-dir2)
	      both (format "^Files %s\\(.+\\) and %s.+ differ$"
			   (regexp-quote smerge-dir1)
			   (regexp-quote smerge-dir2)))
      (setq only-1 (format "^Only in %s:? *\\(.*\\)$" (regexp-quote smerge-dir1))
	    only-2 (format "^Only in %s:? *\\(.*\\)$" (regexp-quote smerge-dir2))
	    both (format "^Files %s\\(.+\\) and %s.+ differ$"
			 (regexp-quote smerge-dir1)
			 (regexp-quote smerge-dir2))))

    ;; Only in 1
    (goto-char (point-min))
    (while (re-search-forward only-1 nil t)
      (setq file (match-string 1))
      (save-match-data
	(loop for exclude in smerge-only-in-excludes while file do
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
    (while (re-search-forward only-2 nil t)
      (setq file (match-string 1))
      (save-match-data
	(loop for exclude in smerge-only-in-excludes while file do
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
    (while (re-search-forward both nil t)
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

(defadvice ediff-quit (after smerge activate)
  (when (overlayp smerge-extent)
    (overlay-put smerge-extent 'face 'smerge-merged-face)
    (delete-other-windows)
    (switch-to-buffer smerge-buffer)
    (let ((next (next-extent smerge-extent))
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

(defun smerge-menu (event)
  "This is called on a right mouse click in the display window.
Pops up a menu that allows copying the file to directory one or two."
  (interactive "e")
  (let ((extent (overlay-at (event-point event))))
    (unless extent (error "No extent at point"))
    (setq smerge-file (smerge-file extent))
    (setq smerge-extent extent)
    (popup-menu smerge-copy-menu)))

;; Find the extent nearest pos. Can return nil.
(defun smerge-nearest-extent (pos)
  (let ((extent (overlay-at pos)))
    (unless extent
      (setq extent (overlay-at (next-overlay-change pos)))
      (unless extent
	(setq extent (overlay-at (previous-overlay-change pos)))
	))
    extent))

(defun smerge-mousable (event)
  "This is called on a left or middle mouse click in the display window."
  (interactive "e")
  (smerge-ediff (smerge-nearest-extent (event-point event))))

(defun smerge-ediff-or-copy ()
  "Ediff or copy the file."
  (interactive)
  (let* ((extent (smerge-nearest-extent (point)))
	 (type (overlay-get extent 'type)))
    (unless extent (error "No extent at point"))
    (cond ((or (eq type 1) (eq type 2))
	   (setq smerge-file (smerge-file extent))
	   (smerge-copy 1 t))
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

(defun smerge-allow-dir (dir)
  "Are we allowed to copy to this directory."
  (let ((type (overylay-get smerge-extent 'type)))
    (if type
	(> (logand (overlay-get smerge-extent 'type) dir) 0)
      (message "WARNING: No type for extent!")
      0)))

;; Copy file preserving the destination modes if necessary
(defun smerge-copy-file (src dst &optional ok-if-already-exists keep-time)
  (let ((modes (file-modes dst)))
    (copy-file src dst ok-if-already-exists keep-time)
    (and smerge-preserve-modes
	 modes
	 (set-file-modes dst modes))))

(defun smerge-copy (dir &optional ask)
  "Do the copy to the directory specified."
  (let ((file1 (concat smerge-dir1 smerge-file))
	(file2 (concat smerge-dir2 smerge-file))
	src dst)
    (cond ((eq dir 1) (setq src file2 dst file1))
	  ((eq dir 2) (setq src file1 dst file2))
	  (t (error "Huh?")))
    (when (or (not ask)
	      (yes-or-no-p (format "Copy to %s? " dst)))
      (smerge-copy-file src dst t t)
      ;; Mark as merged
      (overlay-put smerge-extent 'face 'smerge-merged-face)
      ;; If this is an "only" mark as copied
      (when (< (overlay-get smerge-extent 'type) 3)
	(overlay-put smerge-extent 'type 0))
      (setq smerge-extent nil)
      )))

(defun smerge-make-extent (start end face)
  (let (extent)
    (setq end (1+ end)) ;; include the NL
    (setq extent (make-overlay start end))
    (overlay-put extent 'face face)
    (overlay-put extent 'mouse-face 'highlight)
    (overlay-put extent 'keymap smerge-keymap)
    extent
    ))

(provide 'smerge)
