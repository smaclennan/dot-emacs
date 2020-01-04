# .emacs.d

This directory tree contains my config files and packages for Emacs.

A brief rundown:

## /
  * init.el
    Should need no introduction.

  * user-init.el (optional)
    This is used to tweak per machine settings such as email address
    and laptop-mode.

  * custom.el (optional)
    Used by Emacs customize.

## lisp/
  * lisp code

## rc/
  * I use Didier Verna's el-rcfiles package
    https://github.com/didierverna/el-rcfiles

## src/
  * non-lisp files associated with emacs in some way

## sys/
  * compat-\<emacs major version\>.el
  * \<system-type\>.el

# Just the code

If you are not me, and you probably are not, you might want the lisp
code but not all of the configuration. So I have a compromise. If you
checkout dot-emacs to another directory, say ~/sam-emacs, you can use
sam-lisp-init to initialize the lisp without the configuration. Just
add something like the following to your init.el:

    (load "~/sam-emacs/lisp/sam-lisp-init")
    (sam-lisp-init)

It can also byte-compile the lisp code. Just type:

    C-u M-x sam-lisp-init

or run:

    (sam-lisp-init t)


You can contact me at seanm at seanm.ca.
