# Non-Lisp Source

This directory contains optional source files and scripts that have
something to do with Emacs.

## 2emacs

Send files to Emacs. If Emacs is running, sends the file to the
running Emacs. If Emacs is not running, starts Emacs in the
background with the file as an argument.

If the path ends in :\<number\> then Emacs will go to that line number
in the file.

If the path ends in :* then the : and everything after it is stripped
off. This allows you to double click on, for example, fgrep -H output
and it will "just work".

2emacs also uses find. If the file doesn't exist, then 2emacs will run
find. If only one occurrence is found, then the file is sent. If
more than one is found, they are just listed.

If no files are found, then 2emacs reports an error unless the `-c`
option is given. If the `-c` option is given then the file name is
sent to Emacs.

## cpuid.c

Tries to mimic the Linux cpuid command's output. The flags portion of
the output is not complete compared to Linux. Only works for x86_64
and arm on QNX.

This command can be used by the sys/\<system-type\>.el files for BSD,
QNX, and Windows.

## diary-cleanup.c

Clean up old entries in the diary file. Old means more than a week.

## ispell.bat + ispell.sh

If you don't have some sort of ispell program installed, these script
files just pretend that every word is spelled correctly. Not very
useful, but shuts up flyspell.
