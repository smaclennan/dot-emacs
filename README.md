# .emacs.d

This directory tree contains my config files and packages for Emacs.

A brief rundown:

## /
  * Emacs standard files
    * init.el
      Should need no introduction.

    * early-init.el
      Introduced in 27.1. Speeds up init times.

    * custom.el (optional)
      Used by Emacs customize.

  * user-init.el (optional)
    This is used to tweak per machine settings such as email address
    and laptop-mode.

## lisp/
  * lisp code. 'Nuff said.

## rc/
  * I use Didier Verna's el-rcfiles package.
    This not only speeds up init times, but groups the config
    variables nicely.
    
    https://github.com/didierverna/el-rcfiles

## src/
  * non-lisp files associated with emacs in some way

## sys/
  * compat-\<emacs major version\>.el
  * \<system-type\>.el

# Just the code

If you are not me, and you probably are not, you might want the lisp
code but not all of the configuration. So I have a compromise. If you
checkout dot-emacs to another directory, say ~/dot-emacs, you can use
sam-lisp-init to initialize the lisp without the configuration. Just
add something like the following to your init.el:

    (load "~/dot-emacs/lisp/sam-lisp-init")
    (sam-lisp-init)

It can also byte-compile the lisp code. Just type:

    C-u M-x sam-lisp-init

or run:

    (sam-lisp-init t)


You can contact me at seanm at seanm.ca.
