(require 'sam-common)

;;; -------------------------------------------------------------------------

(defvar include-list
  '("stdio.h" "stdlib.h" "stdint.h" "string.h" "unistd.h" "fcntl.h" "ctype.h" "errno.h")
  "* List of include files added by `c-template'.")

(defvar c++-extra
  '("#include <iostream>" "using namespace std;")
  "* List of lines added by `c-template' for c++ files after the
`include-list' includes.")

;;;###autoload
(defun c-template (&optional getopt)
  (interactive "P")
  (goto-char (point-min))
  (dolist (include include-list)
    (insert (concat "#include <" include ">\n")))
  (when (eq major-mode 'c++-mode)
    (dolist (include c++-extra)
      (insert (concat include "\n"))))

  (when getopt (insert "\n\nstatic int verbose;\n"))
  (insert "\n\nint main(int argc, char *argv[])\n{\n\t")
  (when getopt
    (insert (concat
	     "int c;\n\n"
	     "\twhile ((c = getopt(argc, argv, \"v\")) != EOF)\n"
	     "\t\tswitch (c) {\n" "\t\tcase 'v':\n"
	     "\t\t\t++verbose;\n" "\t\t\tbreak;\n"
	     "\t\tdefault:\n" "\t\t\tputs(\"Sorry!\");\n"
	     "\t\t\texit(1);\n" "\t\t}\n" "\n\t")))
  (let ((mark (point)))
    (insert "\n\treturn 0;\n}\n")
    (goto-char mark))
  (add-local-compile-command nil))

;;; -------------------------------------------------------------------------
;; Not really C specific... but fits here

;;;###autoload
(defun makefile-template (prog)
  (interactive "sProg: ")
  (goto-char (point-min))

  (insert ".PHONY: all clean\n\n")

  (insert "# If you set D=1 on the command line then $(D:1=-g) returns -g,\n"
	  "# else it returns the default (-O2).\n"
	  "D = -O2\n"
	  "CFLAGS += -Wall $(D:1=-g)\n\n")

  (insert "# If you set V=1 on the command line then you will get the actual\n"
	  "# commands displayed.\n"
	  "V	      = @\n"
	  "Q	      = $(V:1=)\n"
	  "QUIET_CC      = $(Q:@=@echo    '     CC       '$@;)\n"
	  "QUIET_RM      = $(Q:@=@echo    '     RM       '$@;)\n\n"
	  ".c.o:\n"
	  "\t$(QUIET_CC)$(CC) -o $@ -c $(CFLAGS) $<\n\n")

  (insert "all: " prog "\n\n"
	  prog ": " prog ".c\n"
	  "\t$(QUIET_CC)$(CC) $(CFLAGS) -o $@ $+\n\n"
	  "clean:\n"
	  "\t$(QUIET_RM)rm -f " prog " *.o\n")
  )

;;; -------------------------------------------------------------------------
(defvar local-compile-cc "gcc -O2 -Wall")
(defvar local-compile-c++ "g++ -O2 -Wall")
(defvar local-compile-go "gccgo")
(defvar local-compile-offset "4")

;; We need to obfuscate this string or Emacs gets confused
(defvar local-vars-str (concat "Local Variables" ":"))

;;;###autoload
(defun add-local-vars (block)
  "Local routine to actually add the block of vars to the file.
Will not overwrite current variables if they exist."
  (save-excursion
    (save-restriction
      (let ((case-fold-search t))

      ;; Make sure local variables do not exist
      (widen)
      (when (search-forward local-vars-str nil t)
	(error "Local variables already exist."))

      ;; Add it
      (goto-char (point-max))
      (insert block)))))

;;;###autoload
(defun add-local-compile-command (arg)
  "Add a local compile command to the current file."
  (interactive "*P")
  (let ((file-name (file-name-nondirectory (buffer-file-name)))
	cmd)

    (cond
     ((eq major-mode 'c-mode)
      (setq cmd (concat local-compile-cc " " file-name " -o "
			(file-name-sans-extension file-name))))
     ((eq major-mode 'c++-mode)
      (setq cmd (concat local-compile-c++ " " file-name " -o "
			(file-name-sans-extension file-name))))
     ((eq major-mode 'go-mode)
      (setq cmd (concat local-compile-go " " file-name " -o "
			(file-name-sans-extension file-name))))
     (t (error "Unsupported mode %S" major-mode)))

    (add-local-vars
     (concat "\n/*\n * " local-vars-str "\n"
	     " * compile-command: \"" cmd "\"\n"
	     (when arg
	       (concat " * indent-tabs-mode: t\n"
		       " * c-basic-offset: " local-compile-offset "\n"
		       " * tab-width: " local-compile-offset "\n"))
	     " * End:\n */\n"))
    (setq-local compile-command cmd)))

;;;###autoload
(defun update-local-compile-command ()
  "If you update the local compile command string, call this to
actually update the associated `compile-command' variable."
  (interactive)
  (save-excursion
    (goto-char (point-max))
    (if (re-search-backward " \* compile-command: \"\\(.*\\)\"" nil t)
	(setq-local compile-command (match-string 1))
      (error "No compile command found."))))

;;;###autoload
(defun set-local-compile-command ()
  (interactive)
  (let ((cmd (read-string "Compile: " compile-command)))
    (setq-local compile-command cmd)))

;;;###autoload
(defun add-local-c-vars (offset)
  "Add local variables to set tab width."
  (interactive "nOffset: ")
  ;; Currently only for C modes
  (unless (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
    (error "Unsupported mode %S" major-mode))
  (setq offset (number-to-string offset))
  (add-local-vars (concat "\n/*\n"
			  " * " local-vars-str "\n"
			  " * indent-tabs-mode: t\n"
			  " * c-basic-offset: " offset "\n"
			  " * tab-width: " offset "\n"
			  " * End:\n"
			  " */\n")))

;;;###autoload
(defun add-local-kernel-vars ()
  "Add local variables to set Linux kernel coding standard."
  (interactive)
  (add-local-c-vars 8))

;;; -------------------------------------------------------------------------

;;;###autoload
(defun open-helper(fname flags &optional mode)
  "Helper for C open() function.
FLAGS are: r = read, w = write, rw = read + write,
	   c = create, t = truncate, a = append.
The x flag adds exit(1) on error, else it returns -1.
And you can add b for binary if you must.
Default FLAGS value is read only."
  (interactive "sFname: \nsFlags [rwctaxb]: ")
  (let (out (rw 0) exit start mark end)
    (dolist (f (string-to-list flags))
      (cond
       ((eq f ?r) (setq rw (logior rw 1)))
       ((eq f ?w) (setq rw (logior rw 2)))
       ((eq f ?c) (setq out (concat out " | O_CREAT") mode "0644"))
       ((eq f ?t) (setq out (concat out " | O_TRUNC")))
       ((eq f ?a) (setq out (concat out " | O_APPEND")))
       ((eq f ?b) (setq out (concat out " | O_BINARY")))
       ((eq f ?x) (setq exit t))
       (t (error "Invalid flag %c" f))))
    (cond
     ((<= rw 1) (setq out (concat "O_RDONLY" out)))
     ((eq rw 2) (setq out (concat "O_WRONLY" out)))
     ((eq rw 3) (setq out (concat "O_RDWR" out))))
    (setq start (point-marker))
    (insert (concat "int fd = open(" fname ", " out
		    (if mode (concat ", " mode))
		    ");\n"
		    "if (fd < 0) {\n"
		    "perror(" fname ");\n"
		    (if exit
			"exit(1);\n"
		      "return -1;\n")
		    "}\n;"))
    (setq mark (point-marker))
    (insert "\nclose(fd);\n")
    (setq end (point-marker))
    (indent-region start end)
    (goto-char mark)
    (delete-char -1)
    ))

;;;###autoload
(defun tabs-vs-spaces (&optional verbose)
  "Does the current buffer use mostly tabs or spaces.

With a prefix-arg displays more verbose output (actual counts),
else just a simple tabs or spaces with a mixed hint.

Non-interactively returns t if using more tabs then
spaces. Biased towards tabs."
  (interactive "P")
  (let ((spaces 0) (tabs 0) (total 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
	(setq total (1+ total))
	(if (eq (char-after) ?\s)
	    ;; Don't count block comments
	    (when (not (eq (char-after (1+ (point))) ?*))
	      (setq spaces (1+ spaces)))
	  (if (eq (char-after) ?\t)
	      (setq tabs (1+ tabs))))
	(forward-line)))
    (when (my-interactive-p)
      (if verbose
	  (message "total %d spaces %d tabs %d = %.0f%% tabs"
		   total spaces tabs (/ (* tabs 100.0) (+ spaces tabs)))
	(message (concat
		  (if (>= tabs spaces) "tabs" "spaces")
		  (if (and (> spaces 0) (> tabs 0)) " (mixed)")))))
    (>= tabs spaces)))

(defvar ifdef-markers '("#if 0 // SAM\n" "#endif\n")
  "List of strings for start and end of ifdef markers.")

;;;###autoload
(defun ifdef-region (start end)
  (interactive "r")
  ;; Convert end offset to marker
  (setq end (copy-marker end))

  (goto-char start)
  (beginning-of-line)
  (insert (car ifdef-markers))

  (goto-char end)
  (unless (bolp)
    (end-of-line)
    (forward-char 1))
  (insert (cadr ifdef-markers)))

(provide 'my-c-helpers)
