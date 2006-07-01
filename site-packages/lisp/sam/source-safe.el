;;; -*- Mode: Emacs-Lisp -*-
;;;
;;; source-safe.el -- SourceSafe in Emacs
;;; Copyright (C) 1996, 1997, 1998 by Rational Software.  All rights reserved.
;;;

(defconst ss-timestamp "Time-stamp: <98/06/25 10:05:21 lanning>")

;;; Commentary:

;;; This file defines a number of functions to manipulate files that are
;;; maintained under the SourceSafe version control system.  SourceSafe is
;;; available from Microsoft, and runs under Windows NT and Win95.  There are
;;; versions of SourceSafe that run on Macs and various flavors of Unix.
;;; More info about SourceSafe can be obtained at the web page
;;;     http://www.microsoft.com/ssafe/

;;;
;;; Authors:
;;;	Stan Lanning <lanning@rational.com>
;;; with contributions from:
;;;     Jake Colman <jcolman@jnc.com>
;;;      - menu additions
;;;	Robert Brodersen <rbrodersen@siebel.com>
;;;	 - support for different SS user name
;;;     Adalbert Bela Homa <belah@nexus.sorostm.ro>
;;;      - baseline files keep the same extension
;;;     Richard Y. Kim <ryk@audiotst.com>
;;;      - suggestion for support of multiple databases
;;;     Matthew Smith <SmithMO@chq.byu.edu>
;;;      - ss-locate()
;;;     Lou Rayman <lrayman@avesta.com>
;;;      - support for passwords
;;;     and many others who pointed out "shortcomings" in earlier versions.

;;; Installation/Usage:
;;;
;;; The only variables that you should futz with are in the
;;; "User definable variables" section early on in the file.
;;; The only variables that you *need* to set are ss-program
;;; and ss-project-dirs.  All the other variables have reasonable
;;; default values.
;;;
;;; For example, you might want to put the following in your
;;; _emacs file:
;;;
;;; (setq ss-program "S:\\WinNT\\SS.exe"
;;;       ss-project-dirs '(("^D:\\\\OCI\\\\" . "$/PurifyW/")))
;;;
;;; (autoload 'ss-diff "source-safe"
;;;  "Compare the current buffer to the version of the file under SourceSafe.
;;;   If NON-INTERACTIVE, put the results in a buffer and switch to that buffer;
;;;   otherwise run ediff to view the differences." t)
;;;
;;; (autoload 'ss-get "source-safe"
;;; "Get the latest version of the file currently being visited." t)
;;;
;;; (autoload 'ss-checkout "source-safe"
;;; "Check out the currently visited file so you can edit it." t)
;;;
;;; (autoload 'ss-lock "source-safe"
;;; "Check out, but don't get the latest version of the file currently being visited." t)
;;;
;;; (autoload 'ss-uncheckout "source-safe"
;;; "Un-checkout the curently visited file." t)
;;;
;;; (autoload 'ss-update "source-safe"
;;; "Check in the currently visited file." t)
;;;
;;; (autoload 'ss-checkin "source-safe"
;;; "Check in the currently visited file." t)
;;;
;;; (autoload 'ss-branch "source-safe"
;;; "Branch off a private, writable copy of the current file for you to work on." t)
;;;
;;; (autoload 'ss-unbranch "source-safe"
;;; "Delete a private branch of the current file.  This is not undoable." t)
;;;
;;; (autoload 'ss-merge "source-safe"
;;; "Check out the current file and merge in the changes that you have made." t)
;;;
;;; (autoload 'ss-history "source-safe"
;;; "Show the checkin history of the currently visited file." t)
;;;
;;; (autoload 'ss-status "source-safe"
;;; "Show the status of the current file." t)
;;;
;;; (autoload 'ss-locate "source-safe"
;;; "Find a file the the current project." t)
;;;
;;; (autoload 'ss-submit-bug-report "source-safe"
;;; "Submit a bug report, with pertinent information." t)
;;;
;;; (autoload 'ss-help "source-safe"
;;; "Describe the SourceSafe mode." t)
;;;
;;; (autoload 'ss-baseline-merge nil nil)
;;; (autoload 'ss-baseline-diff nil nil)
;;;

;;;
;;; Other fancy things you can do
;;;
;;; If you have perl available, try placing a shortcut to
;;; the following script in your "SendTo" folder.
;;; That lets you right-click on a -baseline file in an
;;; Explorer window and do an ss-diff on the corresponding
;;; file.  Yes, that's a pretty terse explanation.  But
;;; you can figure it out.
;;; 
;;;   @rem = '
;;;   @echo off
;;;   @rem Note -- assumes that perl is on your path
;;;   perl -s %0 "ss-baseline-diff" "%1"
;;;   goto endofperl
;;;   @rem ';
;;; # perl
;;;
;;; $emacsFn = $ARGV[0];
;;; $cmd = $ARGV[1];
;;; $cmd =~ s@\\@\\\\@g;
;;; $cmd = "(progn (raise-frame) (" . $emacsFn . " (ss-external-filename (symbol-name '" . $cmd . "))))";
;;;
;;; # Note - assumes that Emacs and gnudoit and the like are already on the path
;;; system "gnudoit", "-q", $cmd;
;;;
;;; __END__
;;;   :endofperl
;;;

(defconst ss-version "v2.0")

;;; ============================================================
;;; User definable variables

(defvar ss-program "SS.exe"
  "The SourceSafe executable.")

(defvar ss-project-dirs '()
  "List associating pathnames with SourceSafe projects.
Each item on the list is a pair of the form (DIR-REGEXP . PROJECT)
where DIR-REGEXP is a regular expression that matches a directory,
and PROJECT is the name of the SourceSafe project that matches that directory.

The DIR_REGEXP can use either / or \\ as a directory separator.

For example, if you have your copy of the SourceSafe project $/MyProj
in the directory D:\\MyProjDir, you would add the pair
    (\"^D:\\\\\\\\MyProjDir\\\\\\\\\" . \"$/MyProj/\")
or
    (\"^D:/MyProjDir/\" . \"$/MyProj/\")
on this list.

Note that each DIR_REGEXP should probably start with a ^,
and that you will probably need to use a large number of \\ characters.

If you change this variable, make sure you clear the variable
ss-project-dirs-cache.")

(defvar ss-database-alist '()
  "List associating pathnames with SourceSafe databases.
This is useful if you are using multiple databases.
Each item on the list is a pair of the form (DIR-REGEXP . DATABASE)
where DIR-REGEXP is a regular expression that matches a directory,
and DATABASE is the value to set the env variable SSDIR to when manipulating
any file that matches DIR_REGEXP.

If NIL (the default), the SSDIR env variable is not modified.

The DIR_REGEXP can use either / or \\ as a directory separator.

Note that each DIR_REGEXP should probably start with a ^,
and that you will probably need to use a large number of \\ characters.")

(defvar ss-tmp-dir nil
  "Directory where Emacs can create SourceSafe temporary directories and files.
If NIL, Emacs will create a temp directory at the root of your copy of the project tree.")

(defvar ss-username nil
  "Source Safe Username.  If nil, your real login name is used.")

(defvar ss-password nil
  "Source Safe Password.  If nil, no password is used.
If non-nil, but not a string, you will be prompted the first time
for the password.")

(defvar ss-update-email-to nil
  "If non-nil, a string specifying whom to send mail to when files are checked in.")

(defvar ss-update-email-cc nil
  "If non-nil, a string specifying whom to cc mail to when files are checked in.")

(defvar ss-update-email-subject "Checkin: >>subject<<"
  "Default update mail subject.")

(defvar ss-update-email-body "
Purpose of checkin:
>>Global check-in message<<

========================================
"
  "Default check-in mail body.")

(defvar ss-confirm-updates nil
  "If true, confirm all UPDATE commands.")

(defvar ss-diff-ignore-whitespace t
  "Should ss-diff ignore whitespace?")

(defvar ss-diff-program '(SS "-DS")
  "External program and arguments to generate diff against the
current version.   The value is a list of the form (PROGRAM . ARGS).
Possible values for PROGRAM and their meanings are:
    SS		-- Use SourceSafe to generate the diff.  The ARGS are
		   passed to SourceSafe.
    DIFF	-- Use the Emacs function (diff old new ARGS).")

(defvar ss-update-new-frame nil
  "If non-null, ss-update will open a new window (Emacs \"frame\") to edit
the check-in message.  The value of ss-update-new-frame will be passed to
the function make-frame to construct the window.")

(defvar ss-ediff-split-window-function
  ;; Default value makes ediff look like SS diff -- side by side
  (if (boundp 'ediff-split-window-function)
      ediff-split-window-function
    'split-window-horizontally)
  "Setting for ediff-split-window-function when invoked by ss commands")

(defvar ss-multiple-checkouts-enabled nil
  "Source-safe has multiple checkouts enabled.")

;;; End of user-settable stuff

(defvar ss-trace nil
  "For debugging purposes.  If true, don't actually do the SourceSafe commands,
instead print what would be done.")


;;; ============================================================
;;; Utility functions

(defun ss-replace-all (s from to)
  "Replace all occurances in the string S of the regexp FROM to the string TO."
  (let ((case-fold-search t)
	pos)
    (while (setq pos (string-match from s))
      (setq s (concat (substring s 0 pos)
		      to
		      (substring s (1+ pos))))))
  s)

(defun ss-string-equal (s1 s2)
  "Like string-equal, but accepts NIL as well as strings, and compares
without regard for case."
  (cond ((null s1) (null s2))
	((null s2) nil)
	(t (string-equal (downcase s1) (downcase s2)))))

(defun ss-regexp-assoc (f alist)
  "Like assoc, but tests uses string-match instead of equal"
  (catch 'converted
    (while alist
      (if (string-match (car (car alist)) f)
	  (throw 'converted (car alist))
	(setq alist (cdr alist))))))

(defun ss-concat-dirname (dir file)
  "Concatenate the directory and file name into a full pathname.
Handles the various cases of dir ending (or not ending) in a delimeter,
and the file beginning (or not beginning) with a delimeter."
  (concat (directory-file-name dir)
	  (make-string 1 directory-sep-char)
	  (if (string-match "^/\\\\" file) (substring file 1) file)))

(defun ss-external-filename (f)
  "Converts a filename to a Windows filename."
  (ss-replace-all
   (or (apply (cond ((fboundp 'win32-long-file-name) ;in emacs 19.34.4
		     'win32-long-file-name)
		    ((fboundp 'w32-long-file-name) ;in emacs 20
		     'w32-long-file-name)
		    (t 'identity))
	      (list f))
       f)
   "/" "\\"))

(defun ss-file-basename (f)
  "Strips of any leading directories and drives from the filename
and returns the result."
  (if (string-match "[/\\\\:]\\([^/\\\\:]+\\)$" f)
      (substring f (match-beginning 1) (match-end 1))
    f))

(defun ss-file-drivename (f)
  "Return the drive name component of a pathname, without a trailing :.
If there is no drive, return NIL."
  (if (string-match "^\\([a-zA-Z]\\):" f)
      (substring f (match-beginning 1) (match-end 1))
    nil))

(defun ss-file-dirname (f)
  "Return the directory component of a pathname, without a trailing delimeter."
  (setq f (expand-file-name f))
  (if (and (file-directory-p f)
	   (not (string-match "[/\\\\]$" f)))
      (setq f (concat f directory-sep-char)))
  (if (string-match "^\\(.*\\)[/\\\\:][^/\\\\:]*$" f)
      (substring f (match-beginning 1) (match-end 1))
    "."))

(defun ss-canonicalize-dirspec-assoc (dirspec)
  (let ((dir (car dirspec))
	(proj (cdr dirspec)))
    (cons (ss-replace-all dir "/" "\\\\")
	  proj)))

(defvar ss-project-dirs-cache nil
  "Cached canonicalized version of ss-project-dirs.
If you change that variable, you should reset this var to NIL.")

(defun ss-project-dirs ()
  (if (null ss-project-dirs-cache)
      (setq ss-project-dirs-cache
	    (mapcar 'ss-canonicalize-dirspec-assoc ss-project-dirs)))
  ss-project-dirs-cache)

(defun ss-username ()
  (or ss-username (user-real-login-name)))

(defun ss-make-file-writable (file writablep)
  (let ((cur-mode (file-modes file))
	(writable-mask 146))		;146 = 444 octal
    (set-file-modes file
		    (if writablep
			(logior cur-mode writable-mask)
		      (logand cur-mode (lognot writable-mask))))))

(defun ss-delete-file (f forcep)
  "Delete the file."
  (if (file-exists-p f)
      (progn
	(if forcep (ss-make-file-writable f t))
	(delete-file f))))

(defun ss-rename-file (old new forcep)
  (ss-delete-file new forcep)
  (rename-file old new forcep))

(defun ss-save-buffer (&optional arg)
  "Like save-buffer, but quiet"
  (if (buffer-modified-p) (save-buffer arg)))

(defun ss-tmp-dir (f)
  "Return a directory to use for temporary files related to the file F."
  (let ((tmp-dir ss-tmp-dir))
    (if (null tmp-dir)
      ;; Try to use a temp dir at the root of the project dir.
      (let ((projects (ss-project-dirs)))
	(while (and projects (null tmp-dir))
	  (if (string-match (concat "^\\(" (car (car projects)) "\\)") f)
	      (setq tmp-dir (substring f 0 (match-end 1)))
	    (setq projects (cdr projects))))))
    (if (null tmp-dir)
	;; Some older versions of SS have a bug in the way
	;; they handle the -GL<path> option to the GET command.
	;; If the <path> starts off with the directory containing
	;; the local copy of the file, SS will effectivly
	;; ignore the path.
	;; So, if we can, try to use the directory "\temp"
	;; on the same drive as the file.
	(if (string-match "^[a-zA-Z]:" f)
	    (setq tmp-dir (concat (substring f 0 2) "\\temp"))))
    (if (null tmp-dir)
	;; That didn't work, so use a dir in the parent dir
	;; (not this dir, due to the above mentioned problem).
	(setq tmp-dir (expand-file-name (ss-concat-dirname (ss-file-dirname f) ".."))))
    ;; Now that we have a tmp-dir, create a dir in it for our own use.
    (if (not (file-exists-p tmp-dir))
	(make-directory tmp-dir))
    (setq tmp-dir (ss-concat-dirname tmp-dir "emacstmp"))
    (if (not (file-exists-p tmp-dir))
	(make-directory tmp-dir))
    tmp-dir))

(defun ss-tmp-file (f)
  "Returns the full pathname of a temp file that does not currently exist.
The temp file will have the same basename as the given file."
  (let ((basename (ss-file-basename f))
	(tmp-dir-root (ss-tmp-dir f))
	(count 0)
	tmp-dir tmpf)
    (while (progn
	     (setq tmp-dir (ss-concat-dirname tmp-dir-root (format "%d" count)))
	     (setq tmpf (ss-concat-dirname tmp-dir basename))
	     (or (file-exists-p tmpf)
		 (and (file-exists-p tmp-dir)
		      (not (file-directory-p tmp-dir)))))
      (setq count (1+ count)))
    (if (not (file-exists-p tmp-dir))
	(make-directory tmp-dir))
    (expand-file-name tmpf)))

(defun ss-modify-file-name (f suffix)
  ;; Append the suffix to the file name.
  ;; Preserve the file extention, if there is one.
  (if (string-match "\\.[^./]+$" f)
      (concat (substring f 0 (match-beginning 0))
	      suffix
	      (substring f (match-beginning 0)))
    (concat f suffix)))

(defun ss-project-filename (f)
  "Convert a filename into the SS project name."
  (setq f (ss-external-filename f))
  (catch 'converted
    (let ((projects (ss-project-dirs)))
      (while projects
	(if (string-match (concat "^\\(" (car (car projects)) "\\)") f)
	    (throw 'converted
		   (let ((proj-root (cdr (car projects)))
			 (proj-file (ss-replace-all (substring f (match-end 1))
						    "\\\\" "/")))
		     (if (string-match "^\\(.*\\)/$" proj-root)
			 (setq proj-root (match-string 1 proj-root)))
		     (if (string-match "^/\\(.*\\)$" proj-file)
			 (setq proj-file (match-string 1 proj-file)))
		     (concat proj-root "/" proj-file))))
	(setq projects (cdr projects))))))

(defvar ss-database-alist-cache nil
  "Cached canonicalized version of ss-database-dirs.
If you change that variable, you shoudl reset this var to NIL.")

(defun ss-database-alist ()
  (if (null ss-database-alist-cache)
      (setq ss-database-alist-cache
	    (mapcar 'ss-canonicalize-dirspec-assoc ss-database-alist)))
  ss-database-alist-cache)

(defun ss-database (f)
  "Lookup the file in the ss-database-alist.
Return the matching database, if found."
  (let ((db (ss-regexp-assoc (ss-external-filename f) (ss-database-alist))))
    (if db (cdr db) nil)))

(defvar ss-quietly t)

(defun ss-log-buffer ()
  (get-buffer-create "*SourceSafe log*"))
  
(defun ss-yes-file ()
  (let ((tmpfile (ss-tmp-file "ssyes.txt")))
    (ss-delete-file tmpfile t)
    (save-excursion
      (set-buffer (generate-new-buffer "*SS Yes*"))
      (erase-buffer)
      (insert "y\n")
      (write-region (point-min) (point-max) tmpfile nil 'no-message)
      (kill-buffer (current-buffer)))
    tmpfile))

(defun ss-compare-files (f1 f2)
  "Compre files f1 and f2.  Return TRUE iff they are identical."
  (let (buf1 buf2 buf1-min buf1-max buf2-min buf2-max)
    (unwind-protect
	(progn
	  (setq buf1 (generate-new-buffer "buf1")
		buf2 (generate-new-buffer "buf2"))
	  (save-excursion
	    (set-buffer buf1)
	    (erase-buffer)
	    (insert-file-contents f1)
	    (setq buf1-min (point-min)
		  buf1-max (point-max)))
	  (save-excursion
	    (set-buffer buf2)
	    (erase-buffer)
	    (insert-file-contents f2)
	    (setq buf2-min (point-min)
		  buf2-max (point-max)))
	  (and (= buf1-max buf2-max)
	       (zerop (compare-buffer-substrings buf1 buf1-min buf1-max
						 buf2 buf2-min buf2-max)))
	  )
      (and buf1 (kill-buffer buf1))
      (and buf2 (kill-buffer buf2)))))

(defun ss-read-password (retryp)
  "Prompt the user for a password, and read it w/o echoing.
If a non-empty password is supplied, return it.  Otherwise return NIL."
  (require 'comint)
  (let ((password (comint-read-noecho
		   (concat (if retryp "Try again. " "")
			   "SourceSafe password for user " (ss-username) ": ") t)))
    (cond ((not (stringp password)) nil)
	  ((string-equal "" password) nil)
	  ((string-equal password
			 (comint-read-noecho "Verify password: " t))
	   password)
	  (t
	   ;; Password mismatch -- try again
	   (ss-read-password t)))))
  
(defun ss-password ()
  (cond ((null ss-password) nil)
	((stringp ss-password) ss-password)
	(t (let ((password (ss-read-password nil)))
	     (if (stringp password)
		 (setq ss-password password)
	       nil)))))


;;; ============================================================
;;; Invoking SourceSafe

;; Some commands are expected to fail -- like doing an
;; ss-update to add a new file to a project.
(defvar ss-errors-ok nil)

(defun ss-insert-quoted-arg (a)
  (if (string-match " " a)
      (insert "\"" a "\"")
    (insert a)))

(defun ss-do-command-1 (infile &rest args)
  (if (not ss-quietly)
      (message "Running SS command %S..." args))
  (let ((buf (ss-log-buffer))
	(dir (expand-file-name default-directory))
	(batfile (ss-external-filename (expand-file-name (ss-tmp-file "ssemacs.bat"))))
	(has-i-option nil)
	(has-o-option nil)
	(outfile nil))

    ;; Create a bat file with the appropriate contents.
    ;; For some reason, this appears to work better than running
    ;; the SS command yourself, probably because I'm using an old
    ;; version of SourceSafe.  Ah well, go figure...

    (save-excursion
      ;(set-buffer (find-file-noselect batfile))
      (set-buffer (generate-new-buffer "*SS command file*"))
      (erase-buffer)

      ;; Make sure we're in the right directory.
      ;; Would love to use "cd /d" but that doesn't work on Win95.
      (let ((drive (ss-file-drivename dir)))
	(if drive (insert drive ":\n")))
      (insert "cd ")
      (ss-insert-quoted-arg (ss-external-filename dir))
      (insert "\n")

      ;; Handle multiple databases
      (let ((db (ss-database dir)))
	(if db
	    (progn
	      (insert "set SSDIR=")
	      (ss-insert-quoted-arg (ss-external-filename db))
	      (insert "\n"))))

      ;; Finally, the Sourcesafe command line
      (ss-insert-quoted-arg (ss-external-filename ss-program))

      (let ((l args))
	(while l
	  (insert " ")
	  (ss-insert-quoted-arg (car l))
	  (cond ((string-match "^-[iI]" (car l))
		 (setq has-i-option t))
		((string-match "^-[oO]" (car l))
		 (setq has-o-option t)))
	  (setq l (cdr l))))

      ;; If there is no infile, and no "-I" option to specify response to queries,
      ;; add a "-I-" option so we won't ever hang
      (cond (infile
	     ;; Presume replies are supplied by the infile
	     (insert " < ")
	     (ss-insert-quoted-arg (ss-external-filename infile)))
	    (has-i-option
	     ;; Found a "-I" option, so nothing else needed
	     )
	    (t
	     ;; No "-I" option
	     (insert " -I-")))

      ;; If there is no outfile, specify one
      (cond ((not has-o-option)
	     (setq outfile (ss-tmp-file "ssoutfile.txt"))
	     (insert " -O@" (ss-external-filename outfile))))

      (insert "\n")
      ;(ss-save-buffer 0)
      (write-region (point-min) (point-max) batfile nil 'no-message)
      (kill-buffer (current-buffer)))
    ;; Do it
    (let ((status (save-excursion
		    (set-buffer buf)
		    (goto-char (point-max))
		    (insert "\n##########"
			    "\n### Executing " batfile "...\n")
		    (call-process batfile nil (list t t) t))))
      ;(sleep-for 20)			;SS returns before it is done!
      (ss-delete-file batfile t)
      (cond ((and outfile (file-exists-p outfile))
	     (save-excursion
	      (set-buffer buf)
	      (goto-char (point-max))
	      (insert-file-contents outfile)
	      (ss-delete-file outfile t))))
      (cond ((zerop status) t)
	    ((= status 1) nil)
	    (t (if (and (not ss-errors-ok)
			(yes-or-no-p (format "SS failed: cmd=%S status=%S.  Do you want to submit a bug report? "
					     args status)))
		   (ss-submit-bug-report))
	       (error "SS failed: cmd=%S status=%S.  See buffer \"%s\" for more info."
		      args status (buffer-name buf)))))))


;; Some SS commands (like ADD) require operating in a particular project.

(defun ss-get-project ()
  (let* ((buf (ss-log-buffer))
	 (p (save-excursion
	      (set-buffer buf)
	      (point-max))))
  (if (ss-do-command-1 nil "PROJECT")
      (save-excursion
	(set-buffer buf)
	(goto-char p)
	(if (search-forward-regexp "^Current project is .*\\(\\$.*\\)$" nil t)
	    (buffer-substring (match-beginning 1) (match-end 1))
	  (error "Can't determine current project"))))))

(defun ss-set-project (project)
  (ss-do-command-1 nil "CP" project))

(defvar ss-ignore-with-project nil)

(require 'backquote)
(defmacro ss-with-project (proj &rest body)
  "Switch to SourceSafe project PROJECT, then execute the remaining BODY args.
Restores the SourceSafe project to the original state, even in case of
abnormal exit.  The value of the last BODY form is returned."
  (let ((orig-proj '__orig_proj__))
    `(let ((,orig-proj (ss-get-project)))
       (if ss-ignore-with-project
	   (progn ,@body)
	 (unwind-protect
	     (if (ss-set-project ,proj) (progn ,@body))
	   (ss-set-project ,orig-proj))))))
(put 'ss-with-project 'lisp-indent-function 1)

(defun ss-do-command (cmd file infile &rest args)
  "Execute the SourceSafe commamd CMD on source file FILE,
with remaining args ARGS.
Note that FILE is not the project name, but the real file name.
Return T if the command succeeded, NIL if it failed gracefully.
If the command failed ungracefully, raise an error."
  (let ((default-directory (concat (ss-file-dirname file)
				   (make-string 1 directory-sep-char))))
    (cond (ss-trace
	   ;; Just print and exit
	   (message "-- SS: cmd=%S file=%S args=%S\n" cmd file args))
	  ((string-equal "ADD" cmd)
	   ;; Requires special handling
	   (ss-with-project
	       (ss-project-filename (ss-file-dirname file))
	     (apply 'ss-do-command-1
		    nil
		    cmd
		    (ss-external-filename file)
		    args)))
	  (t
	   ;; Normal command
	   (let ((proj-file (ss-project-filename file)))
	     (cond ((stringp proj-file) )
		   ((string-equal cmd "STATUS") (setq proj-file "$/"))
		   ((string-equal cmd "LOCATE") (setq proj-file file))
		   (t (error "No project file found for %s" file)))
             (if ss-username
               (let ((y-str (concat "-Y" ss-username))
		     (password (ss-password)))
                 (if password
                     (setq y-str (concat y-str "," password)))
                 (setq args (cons y-str args))))
	     (apply 'ss-do-command-1
		    infile
		    cmd
		    proj-file
		    args))))))

(defun ss-use-dir-options (f)
  (list (concat "-GL" (ss-external-filename (ss-file-dirname f)))
	"-GRN"				;force Windoze EOL convention
	))

;;; ============================================================
;;; Getting info about the current state of a file

(defconst ss-date-string-regexp "[0-9]+/[0-9]+/[0-9]+")
(defconst ss-time-string-regexp "[0-9]+:[0-9][0-9][ap]")

(defun ss-parse-date-time (date time)
  ;; Don't parse, but do canonicalize, at least for now
  (concat date " " time))

(defun ss-current-status (f)
  "Return a list of the form
    (modification-date version (owner check-out-version check-out-date)
                               (owner check-out-version check-out-date) ...)
describing the state of the file."
  (let ((buf (ss-log-buffer))
	(val nil)
	p)
    (setq p (save-excursion (set-buffer buf) (point-max)))
    (ss-do-command "STATUS" f nil)
    (save-excursion
      (set-buffer buf)
      (let ((case-fold-search t)
	    (pattern (concat "^"
			     ;; SourceSafe doesn't always put the entire filename
			     ;; in the output.  Sometimes it gets truncated.
			     (let ((max-test-len 12)
				   (base (ss-file-basename f)))
			       (if (< max-test-len (length base))
				   (concat (regexp-quote (substring base 0 max-test-len))
					   "\\S-*")
				 (regexp-quote base)))
			     ;; Match the user name with a group so it can be easily extracted.
			     ;; User names sometimes have spaces in them, so this is pretty ugly.
			     "\\s-*\\(\\S-+\\(\\s-\\S-+\\)*\\)\\s-"
;			     "\\s-*\\(\\S-[^0-9]*\\S-\\)\\s-"
;			     "\\s-*\\(\\S-+\\)"
			     )))
	(goto-char p)
	(while (search-forward-regexp pattern nil t)
	  (let* ((eol-pos (save-excursion (end-of-line 1) (point)))
		 (owner (buffer-substring (match-beginning 1) (match-end 1)))
		 (check-out-version (if (save-excursion (goto-char (match-end 1)) (looking-at "\\s-+\\(v\\S-+\\)\\s-+"))
					(buffer-substring (match-beginning 1) (match-end 1))
				      nil))
		 (owner-date (if (search-forward-regexp (concat "\\(" ss-date-string-regexp "\\)"
								"\\s-+"
								"\\(" ss-time-string-regexp "\\)"
								)
							eol-pos t)
				 (ss-parse-date-time 
				  (buffer-substring (match-beginning 1) (match-end 1))
				  (buffer-substring (match-beginning 2) (match-end 2)))
			       (ss-parse-date-time nil nil))))
	    (setq val (cons (list (downcase owner) check-out-version owner-date) val))
	    (goto-char eol-pos)))))
    (setq p (save-excursion (set-buffer buf) (point-max)))
    (ss-do-command "HISTORY" f nil "/#1")
    (save-excursion
      (set-buffer buf)
      (goto-char p)
      (setq val (append
		 (if (search-forward-regexp (concat "Version\\s-+\\(\\S-+\\)"
						    ".*\n.*"
						    "Date:\\s-*\\(" ss-date-string-regexp "\\)"
						    "\\s-+"
						    "Time:\\s-+\\(" ss-time-string-regexp "\\)") nil t)
		     (list (ss-parse-date-time (buffer-substring (match-beginning 2) (match-end 2))
					       (buffer-substring (match-beginning 3) (match-end 3)))
			   (concat"v" (buffer-substring (match-beginning 1) (match-end 1))))
		   (list (ss-parse-date-time nil nil) nil))
		 val)))

    val))

(defun ss-user-owns-file-p (status)
  ;; Returns NIL if the current user doesn't have the file checked out.
  ;; Otherwise, return a list (username version timestamp) describing
  ;; just what version is checked out.
  ;; A version of NIL means the current version.
  (let ((user (ss-username))
	(owners (cdr (cdr status))))
    (while (and owners (not (ss-string-equal (car (car owners)) user)))
      (setq owners (cdr owners)))
    (car owners)))

(defun ss-file-is-checked-out-p (status)
  (not (null (cdr (cdr status)))))

(defun ss-current-owners (status)
  (mapcar 'car (cdr (cdr status))))

(defun ss-file-changed-behind-your-back (file status)
  (let ((user-status (ss-user-owns-file-p status)))
    (cond (user-status (not (null (nth 1 user-status))))
	  (t (let ((cur (ss-get-current-version file))
		   (base (ss-get-baseline file)))
	       (unwind-protect
		   (not (ss-compare-files cur base))
		 (ss-delete-file cur t)))))))


;;; ============================================================

(defun ss-get-version (f &optional version)
  "Get a copy of the given version of the file F,
returning the name of the new file.
If VERSION is NIL, get the most recent version."
  (let* ((tmp-file (ss-tmp-file f))
	 (new-name (ss-modify-file-name tmp-file (or version "-latest")))
	 (options (ss-use-dir-options tmp-file)))
    (if (and version (string-match "[v]\\([0-9]+\\)" version))
	(setq options (cons (concat "-V"
				    (substring version
					       (match-beginning 1)
					       (match-end 1)))
			    options)))
    (apply 'ss-do-command "GET" f nil options)
    (if (not (file-exists-p tmp-file))
	(error "SS GET failed: %S not created" tmp-file)
      (ss-rename-file tmp-file new-name t)
      new-name)))

(defun ss-get-current-version (f)
  "Get a copy of the currently checked in version of the file F,
returning the name of the new file."
  (ss-get-version f nil))

(defun ss-project-file-exists-p (f)
  "Return TRUE iff the file exists in SourceSafe"
  (let ((existsp t))
    (condition-case c
	(let ((ss-errors-ok t))
	  (ss-do-command "STATUS" f nil))
      (error (setq existsp nil)))
    existsp))
    

(defun ss-file-modified-p (f)
  "Is the file modified (not the same as the currently checked in version)?"
  (let ((ss-quietly t))
    (not (ss-do-command "DIFF" f nil "-B"))))

(defun ss-file-checked-out-p (f)
  "Return true iff the file is currently checked out by us."
  ;; Check first to see if it is writable.  That way we
  ;; avoid touching SourceSafe in the trivial case, which
  ;; let's us run on a disconnected laptop.
  (and (file-writable-p f)
       (ss-user-owns-file-p (ss-current-status f))))

(defun ss-verify-test (forcep fmt &rest args)
  (or (and (integerp forcep)
	   (yes-or-no-p (apply 'format fmt args)))
      (and (not (integerp forcep))
	   (not (null forcep)))))

(defun ss-verify-already-locked (status forcep msg)
  (cond ((not (ss-file-is-checked-out-p status)) t)
	((not ss-multiple-checkouts-enabled) nil)
	(t (ss-verify-test forcep
			   "File is being worked on by %s. %s? "
			   (ss-current-owners status)
			   msg))))

(defun ss-verify-overwrite (f &optional forcep)
  "Verify that the user wants to overwrite the file.
If the optional FORCEP is nil, and the file has been modified, don't do it.
If FORCEP is an integer, and the file has been modified, ask before clobbering it.
Otherwise, just lose any changes without asking."
  (or (not (ss-file-modified-p f))
      (ss-verify-test forcep
		      "%s differs from the checked-in file.  Replace your copy? " f)))

(defvar ss-update-mail-buffer nil)

(defun ss-build-checkin-message (buf comments-file new-file-p)
  "Build up a SourceSafe check-in mail message."
  (if ss-update-email-to
      (let ((pop-up-windows t)
	    (special-display-buffer-names nil)
	    (special-display-regexps nil)
	    (same-window-buffer-names nil)
	    (same-window-regexps nil)
	    (buf-name "*SS checkin mail*"))
	(require 'sendmail)
	(pop-to-buffer buf-name)
	;; Make sure the SS checkin message buffer is initialized
	(if (not (equal ss-update-mail-buffer (current-buffer)))
	    (progn
	      (mail-mode)
	      (mail-setup ss-update-email-to
			  ss-update-email-subject
			  nil		;in-reply-to
			  ss-update-email-cc
			  nil		;replybuffer
			  '((ss-update-message-sent)))
	      (goto-char (point-max))
	      (insert ss-update-email-body)))
	(setq ss-update-mail-buffer (current-buffer))
	;; Add info about this checkin
	(goto-char (point-max))
	(insert "\n" (ss-project-filename (buffer-file-name buf)))
	(if new-file-p (insert "\t** NEW FILE **"))
	(insert "\n")
	(let ((comment-start (point)))
	  (insert-file-contents comments-file)
	  (goto-char (point-max))
	  (insert "\n")
	  (indent-rigidly comment-start (point) 4)))))

(defun ss-update-message-sent ()
  "The SS checkin message has just been sent."
  (if (buffer-live-p ss-update-mail-buffer)
      (save-excursion
	(set-buffer ss-update-mail-buffer)
	(rename-buffer "*SS checkin mail (sent)*" t)
	(setq ss-update-mail-buffer nil))))

;;; ============================================================

(defun ss-find-file (filename path)
  ;; Search for the filename on the path.
  ;; If found, return the first one found.
  ;; Otherwise, return NIL.
  ;; Implementation copied from the function executable-find
  ;; in executable.el
  (let (file)
    (while path
      (setq path (if (and (setq file (expand-file-name filename (car path)))
			  (file-exists-p file)
			  (not (file-directory-p file)))
		     nil
		   (setq file nil)
		   (cdr path))))
    file))

(eval-when-compile (require 'ediff))

(defun ss-ediff-prepare ()
  (require 'ediff)
  ;; If running on windows, make sure that the diff program is around
  (if (and (memq system-type '(windows-nt windows-95))
	   (not (ss-find-file (if (string-match ".*\\..*" ediff-diff-program)
				  ediff-diff-program
				(concat ediff-diff-program ".exe"))
			      exec-path)))
      (error "Diff program not found.  Versions of diff can be found at %s and %s. See the NTEmacs web page %s for more information"
	     "http://www.uio.no/~andreass/unix_tools.html"
	     "http://www.cygnus.com/misc/gnu-win32"
	     "http://www.cs.washington.edu/homes/voelker/ntemacs.html#other-tools for more information")))

(defun ss-ediff-cleanup-fn (f)
  (condition-case c
      (let (b)
	(while (setq b (find-buffer-visiting f))
	  (kill-buffer b))
	(if (file-exists-p f) (ss-delete-file f t)))
    (error (message "Error during ediff cleanup: %S" c))))

(defun ss-make-ediff-cleanup (form)
  `(lambda ()
     (make-local-variable 'ediff-cleanup-hook)
     (setq ediff-cleanup-hook
	   (cons (lambda () ,form)
		 ediff-cleanup-hook))))

(defun ss-do-ediff-files (basefile newfile &rest cleanup-forms)
  (ss-ediff-prepare)
  (let ((ediff-split-window-function ss-ediff-split-window-function))
    (ediff-files
     ;; Ediff files is proper order so, if viewed side-by-side,
     ;; they appear in the same relative position that the SS
     ;; diff viewer uses.
     basefile newfile
     (list
      (setq ediff-ignore-similar-regions (or ss-diff-ignore-whitespace
					     ediff-ignore-similar-regions))
      (ss-make-ediff-cleanup (cons 'progn cleanup-forms))))))

(defun ss-do-diff-files (basefile newfile diff-program)
  (let ((buf (generate-new-buffer
	      (concat "*DIFF " (ss-file-basename newfile) "*"))))
    (diff basefile newfile (cdr diff-program))))


;;; ============================================================
;;; Public interface functions


;;; ###autoload
(defun ss-diff (compare-to-baseline-p non-interactive-p)
  "Compare the current buffer to the version of the file under SourceSafe.
If the file is a branched file, and COMPARE-TO-BASELINE-P is TRUE,
the buffer is compared to the baseline created when the file was branched.
Otherwise the buffer is compared to the current checked-in version of the file.
If NON-INTERACTIVE-P, put the results in a buffer and switch to that buffer;
otherwise run ediff to view the differences."
  (interactive (let* ((f (buffer-file-name))
		      (fbase (ss-baseline-name f)))
		 (list
		  (and (file-exists-p fbase)
		       (y-or-n-p "Compare buffer to baseline? "))
		  current-prefix-arg)))
  (message "Starting DIFF...")
  (ss-save-buffer)
  (let ((b (funcall (if compare-to-baseline-p 'ss-changes-file 'ss-diff-file)
		    (buffer-file-name) (not non-interactive-p))))
    (if (bufferp b)
	(progn
	  (pop-to-buffer b)
	  (goto-char (point-min))))))

(defun ss-diff-file (f interactivep)
  "Diff the file F against the current version in SourceSafe."
  (cond (interactivep
	 ;; Use EDIFF to browse the diff
	 (let ((ss-file (ss-get-current-version f)))
	   (ss-do-ediff-files ss-file f `(ss-ediff-cleanup-fn ',ss-file))
	   ;; Return NIL so ss-diff knows that the diff has been done
	   nil))

	((eq (car ss-diff-program) 'DIFF)
	 (ss-do-diff-files (ss-get-current-version f) f (cdr ss-diff-program)))

	((eq (car ss-diff-program) 'SS)
	 (let ((tmpfile (ss-tmp-file "ssdiff.txt"))
	       (buf (generate-new-buffer
		     (concat "*DIFF " (ss-file-basename f) "*"))))
	   (apply 'ss-do-command "DIFF" nil f
		  (concat "-O@" (ss-external-filename tmpfile))
		  (append (if ss-diff-ignore-whitespace '("-IW") nil)
			  (cdr ss-diff-program)))
	   (if (file-exists-p tmpfile)
	       (save-excursion
		 (pop-to-buffer buf)
		 (erase-buffer)
		 (insert-file-contents tmpfile)
		 (ss-delete-file tmpfile t)
		 (goto-char (point-min))
		 buf))))

	(t
	 (error "Unknown diff program %S" ss-diff-program))))

(defun ss-changes-file (f interactivep)
  "Diff the file F against the baseline version of the file"
  (let ((fbase (ss-baseline-name f)))
    (cond ((not (file-exists-p fbase))
	   (error "%S is not a branched file" f))

	  (interactivep
	   (ss-do-ediff-files fbase f))

	  ((eq (car ss-diff-program) 'DIFF)
	   (ss-do-diff-files fbase f (cdr ss-diff-program)))

	  (t
	   (error "Unknown diff program %S" ss-diff-program)))))

(eval-when-compile (require 'dired))

;;; ###autoload
(defun ss-get ()
  "Get the latest version of the file currently being visited."
  (interactive)
  (message "Starting GET...")
  (cond ((and (eq major-mode 'dired-mode)
	      (ss-verify-test 0 "Really update the entire tree %s? "
			      dired-directory))
	 (save-excursion (pop-to-buffer (ss-log-buffer)))
	 (if (unwind-protect
		 (ss-get-directory dired-directory t)
	       (revert-buffer t t))
	     (message "SS GET complete.")
	   (message "SS GET failed.")
	   (beep)))
	((and (not (eq major-mode 'dired-mode))
	      (progn (ss-save-buffer) (ss-get-file (buffer-file-name) 0)))
	 (revert-buffer t t)
	 (message "SS GET complete.  If you want to get the lock on the file, use ss-checkout."))
	(t (message "SS GET canceled.")
	   (beep))))

(defun ss-get-file (f &optional forcep)
  "Get the latest version of the file F.
Return true iff the GET succeeded."
  (cond ((ss-verify-overwrite f forcep)
	 (if (file-exists-p f)
	     (ss-delete-file f t))
	 (apply 'ss-do-command "GET" f nil "-I-Y" (ss-use-dir-options f))
	 t)
	(t nil)))

(defun ss-get-directory (d &optional forcep)
  "Get the latest version of all the files in directory D.
Return true iff the GET succeeded."
  (cond ((ss-verify-test forcep "Really update the entire tree %s? " d)
	 (ss-do-command "GET" d nil "-R" "-I-N")
	 t)
	(t nil)))

;;; ###autoload
(defun ss-checkout ()
  "Check out the currently visited file so you can edit it."
  (interactive)
  (message "Starting CHECKOUT...")
  (ss-save-buffer)
  (cond ((ss-checkout-file (buffer-file-name) 0)
	 (revert-buffer t t)
	 (message "SS CHECKOUT complete.  Use ss-update to check it back in."))
	(t (message "SS CHECKOUT canceled.")
	   (beep))))

(defun ss-checkout-file (f &optional forcep)
  "Check out the file F so you can edit it."
  (let ((status (ss-current-status f)))
    (cond ((ss-user-owns-file-p status)
	   (error "You already have the file locked!"))
	  ((not (ss-verify-already-locked status forcep
					  "Check it out anyway"))
	   (error "File is currently locked by %s." (ss-current-owners status)))
	  ((ss-verify-overwrite f forcep)
	   (message "Getting latest version of %s..." f)
	   (apply 'ss-do-command "CHECKOUT" f nil "-I-Y" (ss-use-dir-options f))
	   t)
	  (t nil))))

;;; ###autoload
(defun ss-lock ()
  "Check out the currently visited file so you can edit it,
but *don't* replace the current copy."
  (interactive)
  (message "Starting CHECKOUT without modifying local file...")
  (ss-save-buffer)
  (cond ((ss-lock-file (buffer-file-name) 0)
	 (revert-buffer t t)
	 (message "SS CHECKOUT complete.  Use ss-update to check it back in."))
	(t (message "SS CHECKOUT canceled.")
	   (beep))))

(defun ss-lock-file (f &optional forcep)
  "Check out the file F so you can edit it,
but *don't* replace the current copy."
  (let ((status (ss-current-status f)))
    (cond ((ss-user-owns-file-p status)
	   (error "You already have the file locked!"))
	  ((not (ss-verify-already-locked status forcep
					  "Lock it anyway"))
	   (error "File is currently locked by %s." (ss-current-owners status)))
	  (t (apply 'ss-do-command "CHECKOUT" f nil "-I-Y" "-G-" (ss-use-dir-options f))
	     t))))


;;; ###autoload
(defun ss-uncheckout ()
  "Un-checkout the currently visited file.
Any changes made to the file will be lost."
  (interactive)
  (message "Starting UNCHECKOUT...")
  (ss-save-buffer)
  (cond ((ss-uncheckout-file (buffer-file-name) 0)
	 (revert-buffer t t)
	 (message "SS UNCHECKOUT complete."))
	(t (message "SS UNCHECKOUT canceled.")
	   (beep))))

(defun ss-uncheckout-file (f &optional forcep)
  "Un-checkout the file F.
If the optional FORCEP is nil, and the file has been modified, don't do it.
If FORCEP is an integer, and the file has been modified, ask before clobbering it.
Otherwise, just lose any changes without asking."
  (let ((status (ss-current-status f)))
    (cond ((not (ss-user-owns-file-p status))
	   (error "You don't have file checked out."))
	  ;((ss-file-is-checked-out-p status)
	  ; (error "File is currently locked by %s." (ss-current-owners status)))
	  ((ss-verify-overwrite f forcep)
	   (let ((tmpfile (ss-yes-file)))
	     (apply 'ss-do-command "UNCHECKOUT" f tmpfile "-I-Y" (ss-use-dir-options f))
	     (ss-delete-file tmpfile t)
	     t))
	  (t nil))))


(defvar ss-before-update-hooks nil
  "Hooks to call when starting a SourceSafe UPDATE command.
When the hooks are called the current buffer is the update message
composition buffer; the variable SS-ORIGINAL-BUFFER points to the
buffer containing the file that will be checked in.")

;;; ###autoload
(defun ss-update ()
  "Check in the currently visted file."
  (interactive)
  (message "Starting UPDATE...")
  (let ((fname (buffer-file-name (current-buffer))))
    ;; Before we get carried away, make sure everything is kosher
    (cond ((ss-project-file-exists-p fname)
	   (let ((status (ss-current-status fname)))
	     (cond ((and (file-exists-p (ss-baseline-name fname))
			 (not (ss-user-owns-file-p status)))
		    (if (yes-or-no-p "You have the file branched.  Do you want to do a merge instead?  ")
			(ss-merge nil)
		      (error "You don't have the file checked out.")))
		   ((not (ss-file-is-checked-out-p status))
		    (error "Nobody has the file checked out!"))
		   ((not (ss-user-owns-file-p status))
		    (error "You don't own the file, %s does!" (ss-current-owners status)))
		   ((not (ss-file-changed-behind-your-back fname status))
		    ;; User checked-out version is given as NIL,
		    ;; which means the current version.
		    (ss-update-1))
		   ((progn (beep) (yes-or-no-p "File was modified behind your back.  Do you want to do a merge instead? "))
		    (save-excursion
		      (ss-convert-checkout-to-branch fname status))
		    (ss-merge-1 t t))
		   (t (error "File has been modified behind your back.")))))
	  ((y-or-n-p "File not in the SS project.  Do you want to create it there? ")
	   (ss-update-1))
	  (t (error "File not found in SourceSafe")))))

(defun ss-update-1 ()
  "Check in the currently visted file."
  (let* ((orig-buffer (current-buffer))
	 (orig-windows (current-window-configuration))
	 (orig-frames (current-frame-configuration))
	 (bufname "*SourceSafe update comment*")
	 (fname (buffer-file-name (current-buffer))))
    (ss-save-buffer)
    (cond ((null ss-update-new-frame)
	   (switch-to-buffer-other-window (get-buffer-create bufname)))
	  ((listp ss-update-new-frame)
	   (select-frame (make-frame ss-update-new-frame))
	   (switch-to-buffer (get-buffer-create bufname)))
	  (t
	   (select-frame (make-frame))
	   (switch-to-buffer (get-buffer-create bufname))))
        (if (and ss-buffer-in-use
	     (not (yes-or-no-p "Update comment buffer in use: steal it? ")))
	(switch-to-buffer (generate-new-buffer bufname)))
    ;; Associate some info with the buffer so we can finish
    ;; with the update when the user is done with the comment.
    (setq ss-original-buffer orig-buffer
	  ss-original-window-config orig-windows
	  ss-original-frame-config orig-frames
	  ss-buffer-in-use t)
    ;; Set up ^C-^C to finish the comments, ^C-^D to show diffs.
    (if (null ss-get-update-message-mode-map)
	(progn
	  (setq ss-get-update-message-mode-map (make-sparse-keymap))
	  (define-key ss-get-update-message-mode-map "\C-c\C-c"
	    'ss-update-finish)
	  (define-key ss-get-update-message-mode-map "\C-c\C-d"
	    'ss-update-diff)))
    (use-local-map ss-get-update-message-mode-map)
    (setq major-mode 'ss-get-update-message-mode)
    (setq mode-name "SS Update message")
    (setq buffer-read-only nil)
    (setq fill-column 72)		;SS seems to wrap at 72 chars when putting comments in file
    (message "Enter check-in message.  Hit ^C-^C when done; kill the buffer to quit; ^C-^D to view diffs.")
    (run-hooks 'ss-before-update-hooks)))

(defvar ss-get-update-message-mode-map nil)

(make-variable-buffer-local 'ss-buffer-in-use)
(make-variable-buffer-local 'ss-original-buffer)
(make-variable-buffer-local 'ss-original-window-config)
(make-variable-buffer-local 'ss-original-frame-config)
(setq-default ss-buffer-in-use nil
	      ss-original-buffer nil
	      ss-original-window-config nil
	      ss-original-frame-config nil
	      )

(defvar ss-after-update-hooks nil
  "Hooks to call when a SourceSafe UPDATE command has completed.
When the hooks are called the current buffer contains the file
that has just been checked in.")

(defun ss-update-finish ()
  (interactive)
  (let* ((comment-file (ss-tmp-file "update.txt"))
	 (orig-buffer ss-original-buffer)
	 (orig-windows ss-original-window-config)
	 (orig-frames ss-original-frame-config)
	 (new-file-p (not (ss-project-file-exists-p (buffer-file-name orig-buffer))))
	 (cmd (if new-file-p "ADD" "UPDATE")))
    (if (or (not ss-confirm-updates)
	    (yes-or-no-p (concat "Really update file "
				 (buffer-file-name orig-buffer)
				 "? ")))
	(progn
	  ;; Trim trailing blank lines,
	  ;; but make sure the buffer ends with a blank like
	  (goto-char (point-max))
	  (if (not (looking-at "$^"))
	      (insert "\n"))
	  (while (progn (goto-char (point-max))
			(next-line -1)
			(looking-at "^$"))
	    (kill-line 1))
	  ;; Write out the check-in comment to the temp file
	  (write-region (point-min) (point-max) comment-file nil 'no-message)
	  (setq ss-buffer-in-use nil)
	  ;; Do the check-in
	  (set-buffer orig-buffer)
	  (apply 'ss-do-command cmd
		 (buffer-file-name)
		 nil
		 (concat "-C@" (ss-external-filename comment-file))
		 (if (not new-file-p) (ss-use-dir-options (buffer-file-name))))
	  (if (and nil (frame-configuration-p orig-frames)) ;causes grief in 19.34.5
	      (set-frame-configuration orig-frames))
	  (if (window-configuration-p orig-windows)
	      (set-window-configuration orig-windows))
	  (revert-buffer t t)
	  (ss-build-checkin-message orig-buffer comment-file new-file-p)
	  (message (format "SS %s complete." cmd))
	  (run-hooks 'ss-after-update-hooks))
      (set-frame-configuration orig-frames)
      (set-window-configuration orig-windows)
      (set-buffer orig-buffer)
      (message (format "SS %s aborted." cmd))
      (beep))))

(defun ss-update-diff (arg)
  (interactive "P")
  ;; Show the user the diffs for file
  (ss-diff-file (buffer-file-name ss-original-buffer) (not arg)))

(defalias 'ss-checkin 'ss-update)

;;; ###autoload
(defun ss-branch ()
  "Branch off a private, writable copy of the current file for you to work on.
You can merge this back in later on by using the function (ss-merge)."
  (interactive)
  (message "Starting BRANCH...")
  (ss-save-buffer)
  (let ((fname (buffer-file-name)))
    ;; Try hard to be able to work off-line.
    ;; If the file isn't writable, just do a local branch
    ;; without querying SourceSafe.
    (if (not (file-writable-p fname))
	(cond ((ss-branch-file fname)
	       (revert-buffer t t)
	       (message "SS BRANCH complete.  Use M-x ss-merge to merge back in your changes, M-x ss-unbranch to revert."))
	      (t (message "SS BRANCH failed.")
		 (beep)))
      (let ((status (ss-current-status fname)))
	(cond ((ss-user-owns-file-p status)
	       (if (yes-or-no-p "File currently checked out.  Convert to a private branch? ")
		   (save-excursion
		     (message "Converting to a local branch...")
		     (ss-convert-checkout-to-branch fname status)
		     (message "Converting to a local branch... done."))
		 (message "SS BRANCH aborted.")
		 (beep)))
	      ((ss-branch-file fname)
	       (revert-buffer t t)
	       (message "SS BRANCH complete.  Use M-x ss-merge to merge back in your changes, M-x ss-unbranch to revert."))
	      (t (message "SS BRANCH failed.")
		 (beep)))))))

;; "Baseline" files for branches and diffs
(defconst ss-baseline-suffix "-baseline")

(defun ss-baseline-name (f)
  ;; Append the ss-baseline-suffix to the file name.
  ;; Preserve the file extention, if there is one.
  (ss-modify-file-name f ss-baseline-suffix))

(defun ss-old-baseline-name (f)
  ;; Older version of ss-baseline-name
  (concat f ss-baseline-suffix))

(defun ss-file-from-baseline (baseline-file)
  ;; Invert (ss-baseline-name ...), returning the original
  ;; file name when given the baseline name.
  (cond ((string-match (concat "^\\(.*\\)"
			       (regexp-quote ss-baseline-suffix)
			       "\\(\\.[^./]+$\\)")
		       baseline-file)
	 ;; Matches, with an extension
	 (concat (substring baseline-file
			    (match-beginning 1)
			    (match-end 1))
		 (substring baseline-file
			    (match-beginning 2)
			    (match-end 2))))
	((string-match (concat "^\\(.*\\)"
			       (regexp-quote ss-baseline-suffix)
			       "$")
		       baseline-file)
	 ;; No extension
	 (substring baseline-file
			    (match-beginning 1)
			    (match-end 1)))
	((not (string-match (concat ".*"
				    (regexp-quote ss-baseline-suffix)
				    ".*")
			    baseline-file))
	 ;; If not a baseline file, just return it
	 baseline-file)
	(t
	 ;; A baseline file, but can't figure out how to match it
	 nil)))

(defun ss-get-baseline (f)
  "Return the name of the basefile file for F if there is one,
otherwise return NIL."
  (let ((base (ss-baseline-name f)))
    (if (and (file-exists-p base)
	     (not (file-directory-p base)))
	base
      ;; Backward compat -- older versions of (ss-baseline-name)
      (let ((base (ss-old-baseline-name f)))
	(if (and (file-exists-p base)
		 (not (file-directory-p base)))
	    base
	  ;; Not found under any version
	  nil)))))

(defun ss-branch-file (f)
  ;; Better not be checked out 
  (let ((baseline-name (ss-baseline-name f)))
    (if (file-exists-p baseline-name)
	(error "Baseline file %s already exists" baseline-name)
      (copy-file f baseline-name 0 t)
      (ss-make-file-writable f t)
      t)))

(defun ss-convert-checkout-to-branch (f status)
  ;; Must already be checked out
  (let ((tmp (concat f "@TMP"))
	(version (if (ss-file-changed-behind-your-back f status)
		     (nth 1 (ss-user-owns-file-p status))
		   nil))
	(baseline (ss-baseline-name f)))
    (if (and (file-exists-p baseline)
	     (not (yes-or-no-p "Baseline file %s already exists.  Delete it and continue? "
			       baseline)))
	(error "Conversion of checkout to local branch aborted."))
    (ss-delete-file tmp t)		;just in case
    (copy-file f tmp)
    (unwind-protect
	(progn
	  (ss-uncheckout-file f t)
	  (ss-rename-file (ss-get-version f version) baseline t))
      (ss-rename-file tmp f t))
    t))
  

;;; ###autoload
(defun ss-unbranch ()
  "Delete a private branch of the current file.
This is not undoable."
  (interactive)
  (message "Starting UNBRANCH...")
  (let* ((f (buffer-file-name))
	 (basef (ss-get-baseline f)))
    (cond ((null basef)
	   (error "No branch found for %s" f))
	  ((and (not (ss-compare-files f basef))
		(not (yes-or-no-p "You have changed the file since you branched.  Discard your changes? ")))
	   ;; Abort right now
	   (beep)
	   (message "SS UNBRANCH canceled."))
	  ((yes-or-no-p "Get latest copy of file? ")
	   ;; Toss the branch, update to latest version
	   (message "Replacing your changes with current version...")
	   (ss-delete-file basef t)
	   (ss-get-file f t)
	   (revert-buffer t t)
	   (message "SS UNBRANCH complete."))
	  (t
	   ;; Toss the branch, put the baseline back
	   (message "Reverting to the baseline...")
	   (ss-rename-file basef f t)
	   (revert-buffer t t)
	   (message "SS UNBRANCH complete.")))))


;;; ###autoload
(defun ss-merge (dont-checkoutp)
  "Check out the current file and merge in the changes that you have made.
See the function ss-branch.
If given a prefix argument, merge in the changes without checking out the file."
  (interactive "P")
  (message "Starting MERGE...")
  (ss-merge-1 0 (not dont-checkoutp)))

(defun ss-merge-1 (forcep checkoutp)
  (ss-save-buffer)
  (let* ((file (buffer-file-name))
	 (status (and file (ss-current-status file))))
    (cond

     ((not file)
      (error "There is no file associated with buffer %s" (buffer-name))
      (beep))

     ((ss-user-owns-file-p status)
      ;; User already owns the file.
      ;; If the file was changed behind our back, try to merge in the changes.
      (cond ((not (ss-file-changed-behind-your-back file status))
	     (message "The file hasn't changed since you did a check-out.")
	     (beep))
	    ((yes-or-no-p "Yes indeed, the file has changed!  Proceed with the merge? ")
	     (save-excursion
	       (message "Converting to a local branch...")
	       (ss-convert-checkout-to-branch file status)
	       (message "Converting to a local branch... done."))
	     (ss-merge-1 t t))
	    (t (error "Merge aborted."))))

     ((and checkoutp
	   (not (ss-verify-already-locked status forcep
					  "Proceed with the merge")))
      (error "Can't merge -- file is currently locked by %s."
	     (ss-current-owners status)))

     ((not checkoutp)
      (error "Merging without checking out is not yet supported"))

     (t
      ;; Now to do the merging
      (ss-ediff-prepare)
      (let* ((modified (ss-modify-file-name file "-modified"))
	     (baseline (ss-get-baseline file))
	     (cleanup (ss-make-ediff-cleanup
		       `(ss-merge-cleanup ',file ',modified ',baseline)))
	     (ediff-split-window-function ss-ediff-split-window-function))
	(if (file-exists-p modified)
	    (ss-rename-file modified (concat modified "-OLD") t))
	(condition-case c
	    (progn
	      (message "Moving your file to %s..." modified)
	      (ss-make-file-writable file t)
	      (ss-rename-file file modified t)
	      (if (ss-checkout-file file t)
		  (condition-case c
		      (progn
			(revert-buffer t t)
			(cond ((not baseline)
			       ;; Two-way merge
			       (message "Starting merge...")
			       (ediff-merge-files file modified (list cleanup)))
			      ((and (ss-compare-files file baseline)
				    (y-or-n-p "No changes checked in since the branch.  Accept all your changes? "))
			       ;; No change since split, so just take the modified version.
			       (ss-delete-file file t)
			       (copy-file modified file t t)
			       (ss-merge-cleanup-files baseline modified)
			       (revert-buffer t t)
			       (message "SS MERGE complete.  Don't forget to UPDATE the file."))
			      (t
			       ;; Do the three-way merge.
			       (message "Starting merge...")
			       (ediff-merge-files-with-ancestor
				file modified baseline (list cleanup))))
			t)
		    (error (message "Error %s. Reverting to modified file."
				    (car c))
			   (ss-uncheckout-file file t)
			   (error "%s" (car c))))
		(error "Can't check out file %s." file)))
	  (error
	   (message "Cleaning up from aborted merge - %s." c)
	   (if (file-exists-p modified) (ss-rename-file modified file t))
	   (ss-make-file-writable file nil)
	   (revert-buffer t t)
	   nil)))))))

(defun ss-merge-cleanup (file modified baseline)
  (let ((merge-buffer (ediff-get-buffer (ediff-char-to-buftype ?c)))
	(filebuf (find-buffer-visiting file)))
    (cond ((not (buffer-live-p merge-buffer))
	   (message "Could not find merge buffer -- you are on your own."))
	  ((yes-or-no-p (format "Save the merged buffer as %s? " file))
	   ;; Save the merge
	   (ss-delete-file file t)
	   (save-excursion
	     (set-buffer merge-buffer)
	     (write-region (point-min) (point-max) file)
	     (if filebuf
		 (progn
		   (set-buffer filebuf)
		   (revert-buffer t t))))
	   ;; Delete temp files and buffers
	   (ss-merge-cleanup-files baseline modified)
	   ;; All done
	   (kill-buffer merge-buffer)
	   (other-window 1)
	   (delete-other-windows)
	   (message "SS MERGE complete.  Don't forget to UPDATE the file."))
	  ((yes-or-no-p "Abort the merge, and uncheckout the file? ")
	   ;; Abort the merge
	   (ss-uncheckout-file file t)
	   (ss-rename-file modified file t)
	   nil)
	  (t
	   ;; Merge OK, but user didn't want it saved.
	   (message "OK, it's up to you to save the merge...")
	   (beep)))))

(defun ss-merge-cleanup-files (baseline modified)
  (if (and baseline
	   (yes-or-no-p
	    (concat "Delete the original baseline file " baseline "? ")))
      (let (buf)
	(ss-delete-file baseline t)
	(while (setq buf (find-buffer-visiting baseline))
	  (kill-buffer buf))))
  (if (yes-or-no-p
       (concat "Delete copy of the modified " modified "? "))
      (let (buf)
	(ss-delete-file modified t)
	(while (setq buf (find-buffer-visiting modified))
	  (kill-buffer buf)))))

(defun ss-merge-file (f dont-checkoutp)
  (find-file f)
  (ss-merge dont-checkoutp))

;;; ###autoload
(defun ss-history ()
  "Show the checkin history of the currently visited file."
  (interactive)
  (message "Starting HISTORY...")
  (let ((f (buffer-file-name))
	(tmpfile (ss-tmp-file "sshist.txt")))
    (ss-do-command "HISTORY" f nil
		   (concat "-O@" (ss-external-filename tmpfile)))
    (if (file-exists-p tmpfile)
	(progn
	  (pop-to-buffer "*SS History*")
	  (erase-buffer)
	  (insert-file-contents tmpfile)
	  (goto-char (point-min))
	  (message "SS HISTORY complete."))
      (message "SS HISTORY failed."))))

;;; ###autoload
(defun ss-status ()
  "Show the status of the current file."
  (interactive)
  (message "Starting STATUS...")
  (let ((f (buffer-file-name))
	(tmpfile (ss-tmp-file "ssstatus.txt")))
    (ss-do-command "STATUS" f nil
		   (concat "-O@" (ss-external-filename tmpfile)))
    (if (file-exists-p tmpfile)
	(progn
	  (pop-to-buffer "*SS Status*")
	  (erase-buffer)
	  (insert-file-contents tmpfile)
	  (let ((s (ss-current-status f)))
	    (goto-char (point-max))
	    (if (not (file-exists-p (ss-baseline-name f)))
		(insert "\n * The file is not branched.\n")
	      (insert "\n * You have the file branched.\n")
	      (if (ss-file-changed-behind-your-back f s)
		  (insert " * The file **has** been changed since you branched.\n")
		(insert " * The file has not been changed since you branched.\n"))))
	  (ss-delete-file tmpfile t)
	  (goto-char (point-min))
	  (message "SS STATUS complete."))
      (message "SS STATUS failed."))))


;;; ###autoload
(defun ss-locate (name)
  "Find a file in SourceSafe"
  (interactive "sFile Name to Search For (wild cards ok): ")
  (let ((tmpfile (ss-tmp-file "ssloc.txt")))
    (message "Looking for files matching %s (this may take a while)..." name)
    (ss-do-command "LOCATE" name nil (concat "-O@" tmpfile))
    (if (file-exists-p tmpfile)
	(progn
	  (pop-to-buffer "*SS Locate*")
	  (erase-buffer)
	  (insert-file tmpfile)
	  (goto-char (point-min))
	  (message "SS LOCATE complete."))
      (message "SS LOCATE failed."))))


(defvar ss-maintainer-address "lanning@rational.com")

;;; ###autoload
(defun ss-submit-bug-report ()
  "Submit a bug report, with pertinent information."
  (interactive)
  (require 'reporter)
  (let ((reporter-prompt-for-summary-p t)
	;; Make sure this is nil, since t
	;; causes trouble.
	(reporter-dont-compact-list nil))
    (reporter-submit-bug-report
     ss-maintainer-address
     (concat "Emacs SS " ss-version)
     (list
      'ss-timestamp
      'ss-program
      'ss-project-dirs
      'ss-database-alist
      'ss-tmp-dir
      'ss-username
      'ss-password
      'ss-confirm-updates
      'ss-update-new-frame
      'ss-diff-program
      'ss-diff-ignore-whitespace
      'ss-ignore-with-project
      'ss-trace
      'ss-quietly
      'ss-multiple-checkouts-enabled
      )
     (lambda ()
       ;; No way to find SS version, so have user do it
       (insert "SourceSafe executable version: " "*** FILL ME IN ***" "\n")
       ;; Guess about the OS
       (cond ((not (eq system-type 'windows-nt))
	      ;; Probably Unix.  What are you doing?
	      (insert "OS: Unix\n"))
	     ((null (getenv "OS"))
	      (insert "OS: Win95\n"))
	     ((and (getenv "WINDIR")
		   (file-directory-p (concat (getenv "WINDIR") "\\fonts")))
	      (insert "OS: NT 4.x\n"))
	     (t
	      (insert "OS: NT 3.x\n")))
       )
     (lambda ()
       ;; Insert some other interesting stuff
       (goto-char (point-max))
       (insert "\n\nSS log buffer:\n")
       (insert "=============\n")
       (insert-buffer (ss-log-buffer))
       )
     (concat "**\n"
	     "** Please fill in the SourceSafe executable version, and make\n"
	     "** sure the operating system info is correct.\n"
	     "**\n"
	     "** Don't forget to provide a detailed description of the problem.\n"
	     "**\n\n"
	     )))
  )

;;; ###autoload
(defun ss-help ()
  "Describe the SourceSafe mode"
  (interactive)
  (pop-to-buffer "*Help*")
  (erase-buffer)
  (insert
"The source-safe package is an interface to SourceSafe from Emacs.

The package defines these top-level interactive functions:

  (ss-get)
      Get the latest version of the file currently being visited.
      If run in a dired buffer, updates the entire directory and any
      sub-directories.

  (ss-checkout)
      Check out the currently visited file so you can edit it.

  (ss-lock)
      Check out the currently visited file so you can edit it,
      but don't replace your current copy of the file.

  (ss-uncheckout)
      Un-checkout the curently visited file.

  (ss-update)
  (ss-checkin)
      Check in the currently visited file.

  (ss-diff compare-to-baseline-p non-interactive-p)
      Compare the current buffer to the version of the file under SourceSafe.
      If the file is a branched file, and COMPARE-TO-BASELINE-P is TRUE,
      the buffer is compared to the baseline created when the file was branched.
      Otherwise the buffer is compared to the current checked-in version of the file.
      If NON-INTERACTIVE-P, put the results in a buffer and switch to that buffer;
      otherwise run ediff to view the differences.

  (ss-branch)
      Branch off a private, writable copy of the current file for you to work on.
      You can merge this back in later on by using the function (ss-merge).
      This makes a note of the current baseline file, so merging can use a
      more intelligent three-way merge.

      If the current file is already checked out, converts the file
      to a private branch and unchecks out the file.

  (ss-unbranch)
      Deletes a private branch, abandoning all changes you have made.

  (ss-merge)
      Check out the current file and merge in the changes that you have made.
      See the function (ss-branch).
      If given a prefix argument, merge in the changes without checking out the file.

  (ss-history)
      Display the SourceSafe checkin history for the current file in a buffer.

  (ss-status)
      Display the SourceSafe status of the current file.

  (ss-locate)
      Find a file in SourceSafe.

  (ss-submit-bug-report)
      Submit a bug report, with pertinent information.

  (ss-help)
      Print out this help text.

There are a number of variables of interest:

  ss-program
      Value is " (format "%S" ss-program) ".
      The pathname of the SourceSafe program.  Default is \"SS.exe\".

  ss-project-dirs
      Specifies how to convert NT file names to project names.  The
      value is a list of pairs of the form
        (FILENAME-REGEXP . PROJECT)
      To create the project filename from a Windows filename, the list
      is searched for the first FILENAME-REGEXP that matches the
      filename.  The part of the filename that matched is then replaced
      by the PROJECT, and then all backslashes are converted to
      forward slashes.

      Note: You must use either the forward slash \\ or the backward
      slash / as the directory sepr character in the FILENAME-REGEXP.

      For example, you might set ss-project-dirs like this:

        (setq ss-project-dirs '((\"^C:\\\\\\\\MyProjDir\\\\\\\\\" . \"$/MyProj/\")
			        (\"^D:\\\\\\\\MyProjDir\\\\\\\\\" . \"$/MyProj/\")
				(\"^E:\\\\\\\\MyProjDir\\\\\\\\\" . \"$/MyProj/\")
				(\"^C:/MyProj/\" . \"$/MyProj/\")
				(\"^D:/MyProj/\" . \"$/MyProj/\")
				(\"^E:/MyProj/\" . \"$/MyProj/\")))

      Beware!  Since those are regexp patterns, you need to have lots of
      \\ characters in there.

  ss-username
      Value is " (format "%S" ss-username) ".
      Your SourceSafe user name.  Defaults to your current login name.

  ss-password
      Value is " (if (stringp ss-password) "******" (format "%S" ss-password)) ".
      Your SourceSafe Password.  If NIL, no password is used.  If a string,
      it will be used as your password.  Otherwise, you will be prompted the
      first time for the password to use.

  ss-database-alist
      List associating pathnames with SourceSafe databases.
      This is useful if you are using multiple databases.
      Each item on the list is a pair of the form (DIR-REGEXP . DATABASE)
      where DIR-REGEXP is a regular expression that matches a directory,
      and DATABASE is the value to set the env variable SSDIR to when manipulating
      any file that matches DIR_REGEXP.

      If NIL (the default), the SSDIR env variable is not modified.

      The DIR_REGEXP can use either the forward slash \\ character or
      the backward slash / as a directory separator.

      Note that each DIR_REGEXP should probably start with a ^,
      and that you will probably need to use a large number of \\ characters.

  ss-update-email-to
      Value is " (format "%S" ss-update-email-to) ".
      If not NULL, when you checkin a file (or should I say \"update\"\?),
      emacs automatically creates a mail message addressed to the value
      of this variable, indicating the file name and the checkin
      message.  Subsequent checkins add their info to the message.

      You might set ss-update-email-to like this:

        (setq ss-update-email-to \"my-proj-eng\")

  ss-update-email-cc
      Value is " (format "%S" ss-update-email-cc) ".
      Like ss-update-email-to, but the default CC: list for mail.

  ss-update-email-subject
      Value is " (format "%S" ss-update-email-subject) ".
      Default text for the subject line for update email messages.

  ss-update-email-body
      Default initial text for the body of update email messages.

  ss-diff-ignore-whitespace
      Value is " (format "%S" ss-diff-ignore-whitespace) ".
      If TRUE (the default), ss-diff will ignore trivial differences like whitespace.

  ss-diff-program
      Value is " (format "%S" ss-diff-program) ".
      External program and arguments to generate diff against the
      current version.   The value is a list of the form (PROGRAM . ARGS).
      Possible values for PROGRAM and their meanings are:
	  SS	-- Use SourceSafe to generate the diff.  The ARGS are
		   passed to SourceSafe.
	  DIFF	-- Use the Emacs function (diff old new ARGS).

  ss-update-new-frame
      Value is " (format "%S" ss-update-new-frame) ".
      If a list, ss-update will open a new window (\"frame\" in Emacs terminology)
      to edit the checkin message, using the value as the parameters to make-frame.
      If T, open a new frame with the default make-frame parameters.
      If NULL, edit the checkin message in the existing frame.
      This can be very useful if you follow ss-update by a call to ss-diff:
      you can then step through the changes to the file as a guide to constructing
      the checkin message.

  ss-tmp-dir
      Value is " (format "%S" ss-tmp-dir) ".
      The name of a directory where Emacs and SourceSafe can create directories
      for temporary files.  If NIL (the default), emacs will create a directory
      at the root of your project tree.  Since SourceSafe sometimes has problems
      dealing with FAT file systems, the default is most certainly what you want.

  ss-before-update-hooks
  ss-after-update-hooks
      Hooks to call before/after a SourceSafe UPDATE command.

  ss-ediff-split-window-function
      Setting for ediff-split-window-function when invoked by ss commands.

  ss-multiple-checkouts-enabled nil
      Source-safe has multiple checkouts enabled.
"
  )
  (goto-char (point-min)))


;;; Helper functions for dealing with baseline files

(defun ss-baseline-merge (baseline-file)
  "Given a baseline file, run ss-merge on the current branched version of the file."
  (let ((f (ss-file-from-baseline baseline-file)))
    (if (and (stringp f) (file-exists-p f))
	(ss-merge-file f nil)
      (error "Can't find file corresponding to %s" baseline-file))))

(defun ss-baseline-diff (baseline-file)
  "Given a baseline file, run ss-diff on the current branched version of the file."
  (let ((f (ss-file-from-baseline baseline-file)))
    (if (and (stringp f) (file-exists-p f))
	(ss-diff-file f t)
      (error "Can't find file corresponding to %s" baseline-file))))


;;; ============================================================
;;; Menus

(unless running-xemacs ;; SAM
(defvar ss-menu-map (make-sparse-keymap "SourceSafe"))

(define-key-after menu-bar-tools-menu [ss]
  (cons "SourceSafe" ss-menu-map) 'vc)

(define-key ss-menu-map [ss-help]       '("Help" . ss-help))
(define-key ss-menu-map [ss-history]    '("Show History" . ss-history))
(define-key ss-menu-map [ss-diff]       '("Show Differences" . ss-diff))
(define-key ss-menu-map [ss-status]     '("Show Status" . ss-status))
(define-key ss-menu-map [separator2]    '("--"))
(define-key ss-menu-map [ss-merge]      '("Merge" . ss-merge))
(define-key ss-menu-map [ss-unbranch]   '("Undo Branch" . ss-unbranch))
(define-key ss-menu-map [ss-branch]     '("Branch" . ss-branch))
(define-key ss-menu-map [separator1]    '("--"))
(define-key ss-menu-map [ss-uncheckout] '("Undo Check Out" . ss-uncheckout))
(define-key ss-menu-map [ss-checkin]    '("Check In" . ss-update))
(define-key ss-menu-map [ss-checkout]   '("Check Out" . ss-checkout))
(define-key ss-menu-map [ss-lock]       '("Lock" . ss-lock))
(define-key ss-menu-map [ss-get]        '("Get" . ss-get))
) ;; SAM

(provide 'source-safe)


;;; Local Variables:
;;; time-stamp-active: t
;;; time-stamp-format: "%02y/%02m/%02d %02H:%02M:%02S %u"
;;; time-stamp-line-limit: 20
;;; End:
