;; This is required so that make-clean will work
(require 'compile)

;; So makefiles get nice compile commands
(require 'my-compile)

(comment-warn 'makefile-mode)
(comment-warn 'makefile-gmake-mode)

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
