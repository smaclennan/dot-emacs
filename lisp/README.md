# Lisp code

This directory contains files I wrote... except:

* rcfiles.el
  * https://github.com/didierverna/el-rcfiles
  
* view-kill.el
  * https://www.emacswiki.org/emacs/view-kill.el

* ws-butler
  * https://github.com/lewang/ws-butler
  * melpa


## iswitchb.elc

You may have noticed a link to iswitchb.elc. From iswitchb.el:

`;; This file is obsolete - use icomplete-mode or ido-mode instead.`

Unfortunately icomplete is very different from iswitchb. Whoever
thought icomplete is a replacement for iswitchb probably also thinks
Windows and Linux are completely equivalent.

I tried to use ido for a year at work, but I found that ido does too
much and I was always fighting it.

So I still use iswitchb. However, because it is in the obsolete
directory you get nasty warnings. By putting a link to iswitchb.elc in
my lisp directory the warnings go away.
