ifeq ($(shell basename $(PWD)),.sxemacs)
EMACS:=sxemacs
else
EMACS:=xemacs
endif

all:
	make EMACS=$(EMACS) -C site-packages/lisp/sam
	make EMACS=$(EMACS) -C site-packages/lisp/introspector
	make EMACS=$(EMACS) -C site-packages/lisp/anti-gnus

clean:
	make -C site-packages/lisp/sam clean
	make -C site-packages/lisp/introspector clean
	make -C site-packages/lisp/anti-gnus clean
